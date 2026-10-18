-- Completion of the standard library's functions, as an 'omnifunc'
-- (<C-x><C-o>, or blink.cmp's `omni` source, nvim-cmp's `omni`...).
--
-- No language server: stdlib/*.scaml are read line by line, once (again
-- when a file changes). Offered: the trait methods (`print`, `map`...) with
-- the signature their trait declares, and the top-level functions (`sum`,
-- `to_matrix`...) with their parameters (their types are inferred by the
-- compiler, written nowhere). Only what the buffer can use: core.scaml,
-- plus the files it `#use`s, and those they `#use` in turn.

local M = {}

-- stdlib/, next to editors/ in the repository this plugin comes from;
-- `vim.g.scaml_stdlib` to point elsewhere.
local function stdlib_dir()
  if vim.g.scaml_stdlib then return vim.fn.expand(vim.g.scaml_stdlib) end
  local here = debug.getinfo(1, "S").source:sub(2)
  -- editors/nvim/lua/scaml/complete.lua -> the repository's root
  local root = vim.fn.fnamemodify(vim.fn.resolve(here), ":h:h:h:h:h")
  return root .. "/stdlib"
end

-- file name -> { mtime, functions, uses }
local cache = {}

-- The `//` lines right above line [i], as one text.
local function doc_above(lines, i)
  local doc = {}
  local j = i - 1
  while j >= 1 and lines[j]:match("^%s*//") and not lines[j]:match("^%s*//%*") do
    table.insert(doc, 1, (lines[j]:gsub("^%s*//%s?", "")))
    j = j - 1
  end
  return table.concat(doc, "\n")
end

local function parse(path, file)
  local lines = vim.fn.readfile(path)
  local functions, uses, impls = {}, {}, {}
  local trait = nil -- the trait being read, between `trait x {` and its `}`
  for i, line in ipairs(lines) do
    local used = line:match('^#use%s+"stdlib/([^"]+)"')
    if used then table.insert(uses, used) end

    local t = line:match("^trait%s+([%w_']+)")
    if t then
      trait = t
    elseif trait and line:match("^}") then
      trait = nil
    elseif trait then
      local name, sig = line:match("^%s*fn%s+([%w_']+)%s+of%s+(.-)%s*$")
      if name then
        table.insert(functions, { name = name, sig = sig, trait = trait, doc = doc_above(lines, i), file = file })
      end
    else
      local name, params = line:match("^fn%s+([%w_']+)%s*(.-)%s*{")
      if name then
        table.insert(functions, { name = name, params = params, doc = doc_above(lines, i), file = file })
      end
      local it, ty = line:match("^impl%s+([%w_']+)%s+of%s+(.-)%s*{")
      if it then
        impls[it] = impls[it] or {}
        table.insert(impls[it], ty)
      end
    end
  end
  return functions, uses, impls
end

local function load(dir, file)
  local path = dir .. "/" .. file
  local stat = vim.uv.fs_stat(path)
  if not stat then return nil end
  local entry = cache[file]
  if not entry or entry.mtime ~= stat.mtime.sec then
    local functions, uses, impls = parse(path, file)
    entry = { mtime = stat.mtime.sec, functions = functions, uses = uses, impls = impls }
    cache[file] = entry
  end
  return entry
end

-- core.scaml and every file reached by `#use` from the buffer.
local function available_files(dir, buf)
  local files, seen = {}, {}
  local function visit(file)
    if seen[file] then return end
    seen[file] = true
    local entry = load(dir, file)
    if not entry then return end
    table.insert(files, entry)
    for _, used in ipairs(entry.uses) do visit(used) end
  end
  visit("core.scaml")
  for _, line in ipairs(vim.api.nvim_buf_get_lines(buf, 0, -1, false)) do
    local used = line:match('^#use%s+"stdlib/([^"]+)"')
    if used then visit(used) end
  end
  return files
end

local function items(prefix)
  local dir = stdlib_dir()
  local files = available_files(dir, 0)
  -- every impl, by trait, whatever file it is in
  local impls = {}
  for _, entry in ipairs(files) do
    for trait, tys in pairs(entry.impls) do
      impls[trait] = impls[trait] or {}
      vim.list_extend(impls[trait], tys)
    end
  end
  local result, seen = {}, {}
  for _, entry in ipairs(files) do
    for _, f in ipairs(entry.functions) do
      if not seen[f.name] and vim.startswith(f.name, prefix) then
        seen[f.name] = true
        local info
        if f.trait then
          info = string.format("fn %s of %s\n\ntrait %s (stdlib/%s)", f.name, f.sig, f.trait, f.file)
          if impls[f.trait] then info = info .. "\nfor: " .. table.concat(impls[f.trait], ", ") end
        else
          info = string.format("fn %s %s\n\nstdlib/%s", f.name, f.params, f.file)
        end
        if f.doc ~= "" then info = info .. "\n\n" .. f.doc end
        table.insert(result, {
          word = f.name,
          kind = "f",
          menu = f.sig or (f.params ~= "" and f.params or "()"),
          info = info,
          dup = 0,
        })
      end
    end
  end
  table.sort(result, function(a, b) return a.word < b.word end)
  return result
end

-- OCaml's own modules (`Array.make`, `List.iter`, `Hashtbl.find`...): SCaml
-- compiles to OCaml, so they are all there. Read from the compiler's .mli
-- files (ocamllsp cannot help: it would parse the .scaml as OCaml).

-- `ocamlc -where`, once; `vim.g.scaml_ocaml_stdlib` to point elsewhere.
local ocaml_dir_cache
local function ocaml_dir()
  if vim.g.scaml_ocaml_stdlib then return vim.fn.expand(vim.g.scaml_ocaml_stdlib) end
  if ocaml_dir_cache == nil then
    local out = vim.fn.trim(vim.fn.system({ "ocamlc", "-where" }))
    if vim.v.shell_error ~= 0 then
      out = vim.fn.trim(vim.fn.system({ "opam", "var", "lib" })) .. "/ocaml"
    end
    ocaml_dir_cache = vim.fn.isdirectory(out) == 1 and out or false
  end
  return ocaml_dir_cache or nil
end

-- module name -> { mtime, values }
local ocaml_cache = {}

-- The top-level `val`/`external`s of an .mli (not those of its submodules:
-- only column 0), with their type (over several lines if need be) and the
-- `(** *)` comment below them.
local function parse_mli(path)
  local lines = vim.fn.readfile(path)
  local values = {}
  local i = 1
  while i <= #lines do
    local kind, name, rest = lines[i]:match("^(%a+)%s+([%l_][%w_']*)%s*:%s*(.-)%s*$")
    if kind == "val" or kind == "external" then
      local ty = { rest }
      i = i + 1
      while i <= #lines and lines[i]:match("^%s+%S") and not lines[i]:match("^%s*%(%*") do
        table.insert(ty, vim.trim(lines[i]))
        i = i + 1
      end
      local sig = table.concat(ty, " "):gsub('%s*=%s*".*$', ""):gsub("%s*%[@.-%]", "")
      sig = vim.trim(sig)
      local doc = {}
      if i <= #lines and lines[i]:match("^%s*%(%*%*") then
        repeat
          table.insert(doc, lines[i])
          i = i + 1
        until doc[#doc]:match("%*%)%s*$") or i > #lines
      end
      local text = table.concat(doc, "\n"):gsub("^%s*%(%*%*%s*", ""):gsub("%s*%*%)%s*$", "")
      table.insert(values, { name = name, sig = sig, doc = text })
    else
      i = i + 1
    end
  end
  return values
end

local function ocaml_values(module)
  local dir = ocaml_dir()
  if not dir then return {} end
  local path = dir .. "/" .. module:sub(1, 1):lower() .. module:sub(2) .. ".mli"
  local stat = vim.uv.fs_stat(path)
  if not stat then return {} end
  local entry = ocaml_cache[module]
  if not entry or entry.mtime ~= stat.mtime.sec then
    entry = { mtime = stat.mtime.sec, values = parse_mli(path) }
    ocaml_cache[module] = entry
  end
  return entry.values
end

-- The modules stdlib.mli re-exports (`module Array = Array`...).
local ocaml_modules_cache
local function ocaml_modules()
  if ocaml_modules_cache then return ocaml_modules_cache end
  local dir = ocaml_dir()
  if not dir then return {} end
  ocaml_modules_cache = {}
  for _, line in ipairs(vim.fn.readfile(dir .. "/stdlib.mli")) do
    local name = line:match("^module%s+(%u[%w_]*)%s*=%s*%u")
    if name then table.insert(ocaml_modules_cache, name) end
  end
  return ocaml_modules_cache
end

local function ocaml_items(module, prefix)
  local result = {}
  for _, v in ipairs(ocaml_values(module)) do
    if vim.startswith(v.name, prefix) then
      local info = string.format("%s.%s : %s", module, v.name, v.sig)
      if v.doc ~= "" then info = info .. "\n\n" .. v.doc end
      table.insert(result, { word = v.name, kind = "f", menu = v.sig, info = info, dup = 0 })
    end
  end
  return result
end

local function module_items(prefix)
  local result = {}
  for _, name in ipairs(ocaml_modules()) do
    if vim.startswith(name, prefix) then
      table.insert(result, { word = name, kind = "m", menu = "OCaml module" })
    end
  end
  return result
end

-- the `Module` before the `.` being completed, between the two calls
local completing_module

-- See `:h complete-functions`.
function M.omnifunc(findstart, base)
  if findstart == 1 then
    local line = vim.api.nvim_get_current_line()
    local col = vim.api.nvim_win_get_cursor(0)[2]
    local start = col
    while start > 0 and line:sub(start, start):match("[%w_']") do start = start - 1 end
    completing_module = line:sub(1, start):match("(%u[%w_]*)%.$")
    return start
  end
  if completing_module then return ocaml_items(completing_module, base) end
  if base:match("^%u") then return module_items(base) end
  return items(base)
end

return M
