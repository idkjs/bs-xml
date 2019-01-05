const path = require("path");

const HtmlWebpackPlugin = require("html-webpack-plugin");
const CopyWebpackPlugin = require("copy-webpack-plugin");

module.exports = {
    mode: process.env.NODE_ENV || "development",
    entry: "./test/TestBrowser.bs.js",
    output: {
        path: path.resolve(__dirname, "test_dist"),
        filename: "test.js"
    },
    plugins: [
        new HtmlWebpackPlugin({
            inject: true
        }),
        new CopyWebpackPlugin([
            { from: 'static', to: "" },
        ])
    ],
    devServer: {
        contentBase: path.join(__dirname, 'test_dist'),
        port: 8086
      }
};