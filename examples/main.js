if (process.env.NODE_ENV === "production") {
  require("./output/bundle");
} else {
  require("./output/Main").main();
}
