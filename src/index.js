'use strict';

require("./styles/styles.scss");

const {Elm} = require('./Main');
var app = Elm.Main.init({flags: null});

const VD_LOCAL_STORAGE_KEY = "vd_persist"

// Saves the model to local storage.
app.ports.saveToLocalStorage.subscribe(function(model) {
  localStorage.setItem(VD_LOCAL_STORAGE_KEY, JSON.stringify(model));
});

// Load the model from localStorage and send message to subscription over
// port.
app.ports.loadFromLocalStorage.subscribe(function() {
  app.ports.onLoadFromLocalStorage.send(localStorage.getItem(VD_LOCAL_STORAGE_KEY) || "")
});

const Range = ace.require('ace/range').Range;

const shiftRange = (startLineNumber, [rangeStartLine, rangeEndLine]) => {
  return [ rangeStartLine - startLineNumber, rangeEndLine - startLineNumber ];
}

const editors = { }

const getAceModeNameFromLanguage = (language) => {

  let langSuffix;

  switch (language) {

    case "JavaScript":
      langSuffix = "javascript"
      break;


    case "TypeScript":
      langSuffix = "typescript"
      break;


    case "Java":
      langSuffix = "java"
      break;


    case "C":
    case "C++":
      langSuffix = "c_cpp"
      break;

    case "C#":
      langSuffix = "csharp"
      break;

    case "Go":
      langSuffix = "golang"
      break;

    default:
      throw "unsupported language error"
  }

  return `ace/mode/${langSuffix}`
}

const renderCodeEditor = (renderConfig) => {

  // Already had same editor, destroy it to cancel previous listeners.
  if (editors[renderConfig.tagId]) {

    const editor = editors[renderConfig.tagId];
    editor.destroy();

    if (renderConfig.rerender) {
      const replacementDiv = document.createElement("pre");
      replacementDiv.setAttribute("id", `editor-${renderConfig.tagId}`);
      editor.container.parentNode.replaceChild(replacementDiv, editor.container);
    }
  }

  window.requestAnimationFrame(() => {
    const editor = ace.edit(`editor-${renderConfig.tagId}`);
    editors[renderConfig.tagId] = editor;

    editor.setReadOnly(true);
    editor.setTheme("ace/theme/ambiance");
    editor.setPrintMarginColumn(120);

    const aceModeName = getAceModeNameFromLanguage(renderConfig.language);

    editor.session.setMode(aceModeName);
    editor.setHighlightActiveLine(false);

    editor.setValue(renderConfig.content.join("\n"), -1);

    if (renderConfig.customLineNumbers) {
      editor.session.gutterRenderer = {
        // desired gutter width in pixels
        getWidth: function(session, lastLineText, config) {
           return lastLineText.length * config.characterWidth
        },
        // text for line number
        getText: function(session, row) {
          const lineNumber =
            renderConfig.customLineNumbers[row]
              ? renderConfig.customLineNumbers[row]
               : ""

          return lineNumber
        }
      }
      editor.renderer.updateFull()
    } else {
      editor.setOption("firstLineNumber", renderConfig.startLineNumber)
    }


    for (let greenRange of renderConfig.greenLineRanges) {
      const [ startLine, endLine ] = shiftRange(renderConfig.startLineNumber, greenRange);
      editor.session.addMarker(new Range(startLine, 0, endLine, 1), "green-line", "fullLine")
    }

    for (let redRange of renderConfig.redLineRanges) {
      const [ startLine, endLine ] = shiftRange(renderConfig.startLineNumber, redRange);
      editor.session.addMarker(new Range(startLine, 0, endLine, 1), "red-line", "fullLine")
    }

  })


}

app.ports.renderCodeEditors.subscribe(function(renderConfigs) {

  window.requestAnimationFrame(() => {
    renderConfigs.map((renderConfig) => {
      renderCodeEditor({ ...renderConfig, rerender: false });
    });
  });

})

app.ports.rerenderCodeEditor.subscribe(function(renderConfig) {
  renderCodeEditor({ ...renderConfig, rerender: true });
})
