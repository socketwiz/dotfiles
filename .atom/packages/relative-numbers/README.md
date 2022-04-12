# relative-numbers package

![Example Screencast](https://github.com/justmoon/relative-numbers/blob/master/screencast.gif?raw=true)

Replaces the regular line numbers with relative numbers. Shows the current line number on the active line.

## Supports vim-mode

In [vim-mode](https://github.com/atom/vim-mode)'s insert mode, line numbers will automatically revert back to absolute.

## Customization

### Change current line color

To change the color for the currently highlighted line, put the following in your stylesheet.

``` less
atom-text-editor::shadow .relative.current-line {
  color: purple
}
```

### Show both absolute and relative numbers

The absolute line numbers are still there, they're just invisible. Put this in your stylesheet to always show them:

``` less
atom-text-editor::shadow .absolute {
  display: inline;
  opacity: 0.5;
  padding-right: 1em;
}
```
