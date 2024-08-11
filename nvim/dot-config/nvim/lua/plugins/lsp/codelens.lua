local M = {}

function M.get_codelens()
  local bufnr = vim.api.nvim_get_current_buf()
  local params = { textDocument = vim.lsp.util.make_text_document_params() }

  vim.lsp.buf_request(bufnr, "textDocument/codeLens", params, function(err, res, ...)
    if err or not res then
      return
    end

    local ns = vim.api.nvim_create_namespace("sfrieds3:lsp_codelens")
    vim.api.nvim_buf_clear_namespace(bufnr, ns, 0, -1)

    -- TODO: implement this
    for _, lens in ipairs(res) do
      P(lens)
    end
  end)
end

M.get_codelens()
return M
