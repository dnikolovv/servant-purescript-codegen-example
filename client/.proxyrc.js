const serveStatic = require('serve-static')

module.exports = function (app) {
  // Use static middleware
  app.use(serveStatic('static'))
}