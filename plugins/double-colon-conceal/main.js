/*
THIS IS A GENERATED/BUNDLED FILE BY ESBUILD
if you want to view the source, please visit the github repository of this plugin
*/

var __defProp = Object.defineProperty;
var __getOwnPropDesc = Object.getOwnPropertyDescriptor;
var __getOwnPropNames = Object.getOwnPropertyNames;
var __hasOwnProp = Object.prototype.hasOwnProperty;
var __export = (target, all) => {
  for (var name in all)
    __defProp(target, name, { get: all[name], enumerable: true });
};
var __copyProps = (to, from, except, desc) => {
  if (from && typeof from === "object" || typeof from === "function") {
    for (let key of __getOwnPropNames(from))
      if (!__hasOwnProp.call(to, key) && key !== except)
        __defProp(to, key, { get: () => from[key], enumerable: !(desc = __getOwnPropDesc(from, key)) || desc.enumerable });
  }
  return to;
};
var __toCommonJS = (mod) => __copyProps(__defProp({}, "__esModule", { value: true }), mod);

// main.ts
var main_exports = {};
__export(main_exports, {
  default: () => DoubleColonConcealPlugin,
  editorConcealPlugin: () => editorConcealPlugin
});
module.exports = __toCommonJS(main_exports);
var import_obsidian = require("obsidian");
var import_view = require("@codemirror/view");
var import_state = require("@codemirror/state");
var import_language = require("@codemirror/language");
var DEFAULT_SETTINGS = {
  editMode: false,
  readReplacement: ":",
  editReplacement: ":"
};
var isValidFieldName = (text) => {
  let squareBrackets = 0;
  let roundBrackets = 0;
  for (let i = 0; i < text.length; i++) {
    switch (text[i]) {
      case "[":
        squareBrackets += 1;
        break;
      case "]":
        squareBrackets = Math.max(0, squareBrackets - 1);
        break;
      case "(":
        roundBrackets += 1;
        break;
      case ")":
        roundBrackets = Math.max(0, roundBrackets - 1);
        break;
    }
  }
  return squareBrackets === 0 && roundBrackets === 0;
};
var includesField = (text) => {
  if (!text.includes("::")) {
    return false;
  }
  const parts = text.split("::");
  if (!parts[0] || !isValidFieldName(parts[0])) {
    return false;
  }
  return true;
};
var concealDoubleColon = (node, replacement) => {
  node.textContent = (node.textContent || "").replace(/::/, replacement || "");
};
function hasOverlap(firstFrom, firstTo, secondFrom, secondTo) {
  return firstFrom <= secondTo && secondFrom <= firstTo;
}
var ConcealWidget = class extends import_view.WidgetType {
  constructor(replacement) {
    super();
    this.replacement = replacement;
  }
  eq() {
    return true;
  }
  toDOM() {
    const span = document.createElement("span");
    span.className = "cm-double-colon-conceal";
    span.textContent = this.replacement || "";
    return span;
  }
  ignoreEvent() {
    return false;
  }
};
function addConcealDecorators(view, replacement) {
  const builder = new import_state.RangeSetBuilder();
  const excludeLines = [];
  const excludeSections = [];
  for (const { from, to } of view.visibleRanges) {
    const startLine = view.state.doc.lineAt(from);
    const endLine = view.state.doc.lineAt(to);
    const selection = view.state.selection.main;
    (0, import_language.syntaxTree)(view.state).iterate({
      from,
      to,
      enter: (node) => {
        if (node.name === "inline-code") {
          excludeSections.push([node.from, node.to]);
        } else if (node.name === "hmd-codeblock") {
          excludeLines.push(node.from);
        }
      }
    });
    for (let ln = startLine.number; ln <= endLine.number; ln++) {
      const line = view.state.doc.line(ln);
      if (hasOverlap(line.from, line.to, selection.from, selection.to)) {
        continue;
      }
      if (excludeLines.includes(line.from)) {
        continue;
      }
      if (!includesField(line.text)) {
        continue;
      }
      const signFrom = line.from + line.text.indexOf("::");
      const signTo = signFrom + 2;
      if (excludeSections.some(
        ([selFrom, selTo]) => hasOverlap(selFrom, selTo, signFrom, signTo)
      )) {
        continue;
      }
      builder.add(
        signFrom,
        signTo,
        import_view.Decoration.replace({
          widget: new ConcealWidget(replacement),
          inclusive: false,
          block: false
        })
      );
    }
  }
  return builder.finish();
}
var editorConcealPlugin = (replacement) => import_view.ViewPlugin.fromClass(
  class {
    constructor(view) {
      var _a;
      this.decorations = (_a = addConcealDecorators(view, replacement)) != null ? _a : import_view.Decoration.none;
    }
    update(update) {
      var _a;
      if (!update.state.field(import_obsidian.editorLivePreviewField)) {
        this.decorations = import_view.Decoration.none;
        return;
      }
      if (update.docChanged || update.viewportChanged || update.selectionSet)
        this.decorations = (_a = addConcealDecorators(update.view, replacement)) != null ? _a : import_view.Decoration.none;
    }
  },
  {
    decorations: (value) => value.decorations
  }
);
function createConcealPostProcessor(replacement, id, active) {
  return function concealPostProcessor(el) {
    if (id !== active.current)
      return;
    const elements = el.querySelectorAll("p, li, h1, h2, h3, h4, h5, h6");
    elements.forEach((element) => {
      if (!element.innerText.includes("::"))
        return;
      let elementPosition = 0;
      let afterStyleTag = false;
      for (const node of Array.from(element.childNodes)) {
        elementPosition++;
        if (node.instanceOf(HTMLBRElement)) {
          elementPosition = 0;
          afterStyleTag = false;
          continue;
        }
        if (elementPosition > 1)
          continue;
        if (node.instanceOf(HTMLDivElement) && (node.className.startsWith("list-") || node.className.includes("collapse-indicator"))) {
          elementPosition--;
          continue;
        }
        if (node.instanceOf(HTMLElement) && ["STRONG", "EM", "MARK", "DEL"].includes(node.tagName) && node.childNodes.length === 1 && node.childNodes[0].instanceOf(Text)) {
          const content = (node.childNodes[0].textContent || "").trim();
          if (!content) {
            elementPosition--;
            continue;
          }
          if (includesField(content)) {
            concealDoubleColon(node.childNodes[0], replacement);
            continue;
          }
          if (isValidFieldName(content)) {
            afterStyleTag = true;
            elementPosition--;
            continue;
          }
        }
        if (node.instanceOf(Text)) {
          const content = (node.textContent || "").trim();
          if (!content) {
            elementPosition--;
            continue;
          }
          if (afterStyleTag) {
            if (content.startsWith("::")) {
              concealDoubleColon(node, replacement);
            }
          } else if (includesField(content)) {
            concealDoubleColon(node, replacement);
          }
        }
      }
    });
  };
}
var DoubleColonConcealPlugin = class extends import_obsidian.Plugin {
  constructor() {
    super(...arguments);
    this.editorExtension = [];
    this.markdownPostProcessorOpts = {
      current: 0
    };
  }
  async onload() {
    await this.loadSettings();
    this.addEditorExtension();
    this.registerEditorExtension(this.editorExtension);
    this.addMarkdownPostProcessor();
    this.addSettingTab(new DoubleColonConcealSettingTab(this.app, this));
    this.app.workspace.updateOptions();
    this.rerenderActiveMarkdownViews();
  }
  onunload() {
    this.app.workspace.updateOptions();
    this.rerenderActiveMarkdownViews();
  }
  rerenderActiveMarkdownViews() {
    var _a;
    (_a = this.app.workspace.getActiveViewOfType(import_obsidian.MarkdownView)) == null ? void 0 : _a.previewMode.rerender(true);
  }
  addEditorExtension() {
    this.editorExtension.length = 0;
    this.app.workspace.updateOptions();
    if (this.settings.editMode) {
      this.editorExtension.push(
        editorConcealPlugin(this.settings.editReplacement)
      );
    }
  }
  addMarkdownPostProcessor() {
    this.markdownPostProcessorOpts.current++;
    this.registerMarkdownPostProcessor(
      createConcealPostProcessor(
        this.settings.readReplacement,
        this.markdownPostProcessorOpts.current,
        this.markdownPostProcessorOpts
      )
    );
  }
  updateEditorExtension() {
    this.addEditorExtension();
    this.app.workspace.updateOptions();
  }
  updateMarkdownPostProcessor() {
    this.addMarkdownPostProcessor();
    this.rerenderActiveMarkdownViews();
  }
  async loadSettings() {
    this.settings = Object.assign({}, DEFAULT_SETTINGS, await this.loadData());
  }
  async saveSettings() {
    await this.saveData(this.settings);
  }
};
var DoubleColonConcealSettingTab = class extends import_obsidian.PluginSettingTab {
  constructor(app, plugin) {
    super(app, plugin);
    this.plugin = plugin;
  }
  display() {
    const { containerEl } = this;
    containerEl.empty();
    containerEl.createEl("h2", { text: "General Settings" });
    new import_obsidian.Setting(containerEl).setName("Conceal double colon in Editing view").setDesc(
      'Double colon is concealed except on an active line or within a text selection. Source mode is also excluded. Concealed double colon has ".cm-double-colon-conceal" CSS class attached that could be used for customization purposes.'
    ).addToggle(
      (toggle) => toggle.setValue(this.plugin.settings.editMode).onChange(async (value) => {
        this.plugin.settings.editMode = value;
        await this.plugin.saveSettings();
        this.plugin.updateEditorExtension();
      })
    );
    containerEl.createEl("h2", { text: "Conceal Character" });
    new import_obsidian.Setting(containerEl).setName("Reading view").setDesc(
      "Double colon will be replaced by this string in the reading view."
    ).addText(
      (text) => text.setValue(this.plugin.settings.readReplacement).onChange(async (value) => {
        this.plugin.settings.readReplacement = value || "";
        await this.plugin.saveSettings();
        this.plugin.updateMarkdownPostProcessor();
      })
    );
    new import_obsidian.Setting(containerEl).setName("Editing view").setDesc(
      "Double colon will be replaced by this string in the editing view."
    ).addText(
      (text) => text.setValue(this.plugin.settings.editReplacement).onChange(async (value) => {
        this.plugin.settings.editReplacement = value || "";
        await this.plugin.saveSettings();
        this.plugin.updateEditorExtension();
      })
    );
  }
};

/* nosourcemap */