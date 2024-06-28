const path = require('path');
const webpack = require('webpack');

const clientConfig = {
  mode: 'production',
  target: 'node',
  entry: './client/src/extension.ts', // Adjust this path according to your entry file
  output: {
    path: path.resolve(__dirname, 'client', 'out'),
    filename: 'extension.js',
    libraryTarget: 'commonjs2'
  },
  resolve: {
    extensions: ['.ts', '.js']
  },
  module: {
    rules: [
      {
        test: /\.ts$/,
        use: 'ts-loader',
        exclude: /node_modules/
      }
    ]
  },
  externals: {
    vscode: 'commonjs vscode' // the vscode-module is created on-the-fly and must be excluded.
  },
  devtool: 'source-map',
  stats: {
    errorDetails: true
  },
  plugins: [
    new webpack.ContextReplacementPlugin(
      /vscode-languageserver-types[\/\\]lib[\/\\]umd/,
      path.resolve(__dirname, 'client')
    )
  ]
};

const serverConfig = {
  mode: 'production',
  target: 'node',
  entry: './server/src/server.ts', // Adjust this path according to your server entry file
  output: {
    path: path.resolve(__dirname, 'server', 'out'),
    filename: 'server.js',
    libraryTarget: 'commonjs2'
  },
  resolve: {
    extensions: ['.ts', '.js']
  },
  module: {
    rules: [
      {
        test: /\.ts$/,
        use: 'ts-loader',
        exclude: /node_modules/
      }
    ]
  },
  externals: {
    vscode: 'commonjs vscode' // the vscode-module is created on-the-fly and must be excluded.
  },
  devtool: 'source-map',
  stats: {
    errorDetails: true
  },
  plugins: [
    new webpack.ContextReplacementPlugin(
      /vscode-languageserver-types[\/\\]lib[\/\\]umd/,
      path.resolve(__dirname, 'server')
    ),
    new webpack.ContextReplacementPlugin(
      /vscode-languageserver-textdocument[\/\\]lib[\/\\]umd/,
      path.resolve(__dirname, 'server')
    )
  ]
};

module.exports = [clientConfig, serverConfig];
