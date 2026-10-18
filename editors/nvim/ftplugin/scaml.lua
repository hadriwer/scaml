-- Same comment/indent conventions as editors/vscode/language-configuration.json.
vim.bo.commentstring = "// %s"
vim.bo.comments = "://"
vim.bo.autoindent = true
vim.bo.smartindent = true
-- the standard library's functions (see lua/scaml/complete.lua)
vim.bo.omnifunc = "v:lua.require'scaml.complete'.omnifunc"

vim.b.undo_ftplugin = "setlocal commentstring< comments< autoindent< smartindent< omnifunc<"
