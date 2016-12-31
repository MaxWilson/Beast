var path = require("path");
var webpack = require("webpack");

var cfg = {
  entry: [
    "./out/source/main.js"
  ],
  output: {
    path: path.join(__dirname, "public"),
    filename: "bundle.js"
  },
  plugins: [
    new webpack.DefinePlugin({
      'process.env': {
        'NODE_ENV': JSON.stringify('production')
      }
    })
  ],
  module: {
    rules: [
      {
        enforce: 'pre',
        test: /\.js$/,
        exclude: /node_modules/,
        loader: "source-map-loader"
      }
    ]
  }
};

module.exports = cfg;