const hljs = require("highlight.js");

module.exports = {
  plugins: {
    "@nonbili/posthtml-md-element": {
      root: __dirname,
      html: true,
      highlight: (str, lang) => {
        if (lang && hljs.getLanguage(lang)) {
          try {
            return (
              '<pre class="hljs ' +
              lang +
              '">' +
              hljs.highlight(lang, str, true).value +
              "</pre>"
            );
          } catch (_) {}
        }

        return '<pre class="hljs">' + str + "</pre>";
      }
    }
  }
};
