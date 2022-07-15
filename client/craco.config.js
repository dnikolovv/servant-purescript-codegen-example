const cracoPureScriptLoader = require("craco-purescript-loader");

module.exports = {
  plugins: [
    {
      plugin: cracoPureScriptLoader,
      options: {
        spago: true,
        pscIde: true,
      },
    },
  ],
};
