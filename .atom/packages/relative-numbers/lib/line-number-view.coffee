{CompositeDisposable} = require 'atom'

module.exports =
class LineNumberView
  constructor: (@editor) ->
    @subscriptions = new CompositeDisposable()
    @editorView = atom.views.getView(@editor)
    @trueNumberCurrentLine = atom.config.get('relative-numbers.trueNumberCurrentLine')
    @startAtOne = atom.config.get('relative-numbers.startAtOne')

    @gutter = @editor.addGutter
      name: 'relative-numbers'
    @gutter.view = this

    # Update line numbers whenever tiles are updated
    @lineNumbersContainer = @editorView.rootElement?.querySelector '.line-numbers'
    @observer = new MutationObserver(@_update)
    @observer.observe(@lineNumbersContainer, childList: true)

    # Subscribe for when the line numbers should be updated.
    @subscriptions.add @editor.onDidChangeCursorPosition(@_update)
    @subscriptions.add @editor.onDidStopChanging(@_update)

    # Subscribe to when the true number on current line config is modified.
    @subscriptions.add atom.config.onDidChange 'relative-numbers.trueNumberCurrentLine', =>
      @trueNumberCurrentLine = atom.config.get('relative-numbers.trueNumberCurrentLine')
      @_update()

    # Subscribe to when the start at one config option is modified
    @subscriptions.add atom.config.onDidChange 'relative-numbers.startAtOne', =>
      @startAtOne = atom.config.get('relative-numbers.startAtOne')
      @_update()

    # Dispose the subscriptions when the editor is destroyed.
    @subscriptions.add @editor.onDidDestroy =>
      @subscriptions.dispose()
      @observer.disconnect()

    @_update()

  destroy: () ->
    @subscriptions.dispose()
    @_undo()
    @gutter.destroy()
    @observer.disconnect()

  _spacer: (totalLines, currentIndex) ->
    width = Math.max(0, totalLines.toString().length - currentIndex.toString().length)
    Array(width + 1).join '&nbsp;'

  # Update the line numbers on the editor
  _update: () =>
    totalLines = @editor.getLineCount()
    bufferRow = @editor.getCursorBufferPosition().row

    # Check if selection ends with newline (line-selection)
    if @editor.getSelectedText().match /\n$/
      selectRange = @editor.getSelectedBufferRange()
      currentLineNumber = bufferRow

      # Check if multiple line selection
      if selectRange.start.row != selectRange.end.row

        # Check if cursor on first or last line (needed for vim-mode line selection)
        if selectRange.start.row == bufferRow
          currentLineNumber = bufferRow + 1
    else
      currentLineNumber = bufferRow + 1

    lineNumberElements = @editorView.rootElement?.querySelectorAll('.line-number')
    offset = if @startAtOne then 1 else 0

    for lineNumberElement in lineNumberElements
      row = Number(lineNumberElement.getAttribute('data-buffer-row'))
      absolute = row + 1
      relative = Math.abs(currentLineNumber - absolute)
      relativeClass = 'relative'
      if @trueNumberCurrentLine and relative == 0
        relative = currentLineNumber - 1
        relativeClass += ' current-line'
      # Apply offset last thing before rendering
      relative += offset
      absoluteText = @_spacer(totalLines, absolute) + absolute
      relativeText = @_spacer(totalLines, relative) + relative

      # Keep soft-wrapped lines indicator
      if lineNumberElement.innerHTML.indexOf('•') == -1
        lineNumberElement.innerHTML = "<span class=\"absolute\">#{absoluteText}</span><span class=\"#{relativeClass}\">#{relativeText}</span><div class=\"icon-right\"></div>"

  # Undo changes to DOM
  _undo: () =>
    totalLines = @editor.getLineCount()
    lineNumberElements = @editorView.rootElement?.querySelectorAll('.line-number')
    for lineNumberElement in lineNumberElements
      row = Number(lineNumberElement.getAttribute('data-buffer-row'))
      absolute = row + 1
      absoluteText = @_spacer(totalLines, absolute) + absolute
      if lineNumberElement.innerHTML.indexOf('•') == -1
        lineNumberElement.innerHTML = "#{absoluteText}<div class=\"icon-right\"></div>"
