let g:OmniSharp_server_stdio = 0
let g:OmniSharp_server_use_mono = 1
let g:OmniSharp_highlight_types = 2
let g:OmniSharp_selector_ui = ''
let g:omnicomplete_fetch_full_documentation = 1

" Show type information automatically when the cursor stops moving
autocmd CursorHold *.cs call OmniSharp#TypeLookupWithoutDocumentation()

" The following commands are contextual, based on the cursor position.
autocmd FileType cs nnoremap <buffer> gd :OmniSharpGotoDefinition<CR>
autocmd FileType cs nnoremap <buffer> <Leader>i :OmniSharpFindImplementations<CR>
autocmd FileType cs nnoremap <buffer> <Leader>s :OmniSharpFindSymbol<CR>
autocmd FileType cs nnoremap <buffer> <Leader>u :OmniSharpFindUsages<CR>

" Finds members in the current buffer
" autocmd FileType cs nnoremap <buffer> <Leader>fm :OmniSharpFindMembers<CR>

" autocmd FileType cs nnoremap <buffer> <Leader>fx :OmniSharpFixUsings<CR>
autocmd FileType cs nnoremap <buffer> <Leader>tt :OmniSharpTypeLookup<CR>
autocmd FileType cs nnoremap <buffer> <Leader>dc :OmniSharpDocumentation<CR>
autocmd FileType cs nnoremap <buffer> <C-\> :OmniSharpSignatureHelp<CR>
autocmd FileType cs inoremap <buffer> <C-\> <C-o>:OmniSharpSignatureHelp<CR>

" Navigate up and down by method/property/field
" autocmd FileType cs nnoremap <buffer> <C-k> :OmniSharpNavigateUp<CR>
" autocmd FileType cs nnoremap <buffer> <C-j> :OmniSharpNavigateDown<CR>

" Find all code errors/warnings for the current solution and populate the quickfix window
autocmd FileType cs nnoremap <buffer> <Leader>cc :OmniSharpGlobalCodeCheck<CR>
autocmd FileType cs nnoremap <Leader>ca :OmniSharpGetCodeActions<CR>
" Run code actions with text selected in visual mode to extract method
autocmd FileType cs xnoremap <Leader>ca :call OmniSharp#GetCodeActions('visual')<CR>
