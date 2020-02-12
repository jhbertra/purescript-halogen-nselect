const purgecss = require("@fullhuman/postcss-purgecss")({
  content: ["./index.html", "./README.md", "./src/**/*.purs"]
});

module.exports = {
  plugins: [
    require("tailwindcss")("./tailwind.config.js"),
    ...(process.env.NODE_ENV === "production" ? [purgecss] : [])
  ]
};
