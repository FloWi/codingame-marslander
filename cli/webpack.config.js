const webpack = require('webpack');
const path = require('path');
const fs = require('fs');

module.exports = require('./scalajs.webpack.config');

module.exports.output.filename = "marslander.js";
module.exports.target = "node";
module.exports.plugins = module.exports.plugins || [];
module.exports.plugins.push(new webpack.BannerPlugin({
  banner: '#!/usr/bin/env -S node --enable-source-maps',
  raw: true,
}));
module.exports.plugins.push(function () {
  this.plugin('done', () => fs.chmodSync('marslander.js', '755'))
});
