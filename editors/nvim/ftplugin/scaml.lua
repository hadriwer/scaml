-- Same comment/indent conventions as editors/vscode/language-configuration.json.
vim.bo.commentstring = "// %s"
vim.bo.comments = "://"
vim.bo.autoindent = true
vim.bo.smartindent = true

vim.b.undo_ftplugin = "setlocal commentstring< comments< autoindent< smartindent<"
