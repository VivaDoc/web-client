const path = require("path");
const webpack = require("webpack");
const merge = require("webpack-merge");

const CopyWebpackPlugin = require("copy-webpack-plugin");
const HTMLWebpackPlugin = require("html-webpack-plugin");
const CleanWebpackPlugin = require("clean-webpack-plugin");
const StringReplacePlugin = require("string-replace-webpack-plugin");

// to extract the css as a separate file
const MiniCssExtractPlugin = require("mini-css-extract-plugin");

const PROD_API_URL = "https://api.vivadoc.io:8888/api"

var MODE =
    process.env.npm_lifecycle_event === "prod" ? "production" : "development";
var filename = MODE == "production" ? "[name]-[hash].js" : "index.js";

var common = {
    mode: MODE,
    entry: "./src/index.js",
    output: {
        path: path.join(__dirname, "dist"),
        publicPath: "/",
        // webpack -p automatically adds hash when building for production
        filename: filename
    },
    plugins: [
        new HTMLWebpackPlugin({
            // Use this template to get basic responsive meta tags
            template: "src/index.html",
            // inject details of output file at end of body
            inject: "body"
        }),
        new StringReplacePlugin()
    ],
    resolve: {
        modules: [path.join(__dirname, "src"), "node_modules"],
        extensions: [".js", ".elm", ".scss", ".png"]
    },
    module: {
        rules: [
            {
                test: /\.js$/,
                exclude: /node_modules/,
                use: {
                    loader: "babel-loader"
                }
            },
            {
                test: /\.scss$/,
                exclude: [/elm-stuff/, /node_modules/],
                // see https://github.com/webpack-contrib/css-loader#url
                loaders: ["style-loader", "css-loader?url=false", "sass-loader"]
            },
            {
                test: /\.css$/,
                exclude: [/elm-stuff/, /node_modules/],
                loaders: ["style-loader", "css-loader?url=false"]
            },
            {
                test: /\.woff(2)?(\?v=[0-9]\.[0-9]\.[0-9])?$/,
                exclude: [/elm-stuff/, /node_modules/],
                loader: "url-loader",
                options: {
                    limit: 10000,
                    mimetype: "application/font-woff"
                }
            },
            {
                test: /\.(ttf|eot|svg)(\?v=[0-9]\.[0-9]\.[0-9])?$/,
                exclude: [/elm-stuff/, /node_modules/],
                loader: "file-loader"
            },
            {
                test: /\.(jpe?g|png|gif|svg)$/i,
                exclude: [/elm-stuff/, /node_modules/],
                loader: "file-loader"
            }
        ]
    }
};

if (MODE === "development") {
    console.log("Building for dev...");
    module.exports = merge(common, {
        plugins: [
            // Suggested for hot-loading
            new webpack.NamedModulesPlugin(),
            // Prevents compilation errors causing the hot loader to lose state
            new webpack.NoEmitOnErrorsPlugin()
        ],
        module: {
            rules: [
                {
                    test: /\.elm$/,
                    loader: StringReplacePlugin.replace({
                        replacements: [
                            {
                                pattern: /__WEBPACK_CONSTANT_API_BASE_URL__/g,
                                replacement: function (match, p1, offset, string) {
                                   return "http://localhost:3001/api";
                                }
                           },
                           {
                               pattern: /__WEBPACK_CONSTANT_OAUTH_CLIENT_ID__/g,
                               replacement: function(match, p1, offset, string) {
                                   return "Iv1.aca2ea3b5a164445";
                               }
                           },
                           {
                               pattern: /__WEBPACK_CONSTANT_GITHUB_APP_NAME__/g,
                               replacement: function(match, p1, offset, string) {
                                   return "vivadocdev"
                               }
                           },
                           {
                               pattern: /__WEBPACK_CONSTANT_REDIRECT_URI__/g,
                               replacement: function(match, p1, offset, string) {
                                   return "http://localhost:8080/oauth_redirect";
                               }
                           }
                       ]
                   })
                },
                {
                    test: /\.elm$/,
                    exclude: [/elm-stuff/, /node_modules/],
                    use: [
                        { loader: "elm-hot-webpack-loader" },
                        {
                            loader: "elm-webpack-loader",
                            options: {
                                // add Elm's debug overlay to output
                                debug: true,
                                forceWatch: true
                            }
                        }
                    ]
                }
            ]
        },
        devServer: {
            inline: true,
            stats: "errors-only",
            contentBase: path.join(__dirname, "src"),
            historyApiFallback: true,
            // feel free to delete this section if you don't need anything like this
            before(app) {
                // on port 3000
                app.get("/test", function(req, res) {
                    res.json({ result: "OK" });
                });
            }
        }
    });
}
if (MODE === "production") {
    console.log("Building for Production...");
    module.exports = merge(common, {
        plugins: [
            // Delete everything from /dist directory and report to user
            new CleanWebpackPlugin(["dist"], {
                root: __dirname,
                exclude: [],
                verbose: true,
                dry: false
            }),
            // Copy static assets
            new CopyWebpackPlugin([
                { from: "src/assets", to: "assets/" },
                { from: "src/ace", to: "ace" }
            ]),
            new MiniCssExtractPlugin({
                // Options similar to the same options in webpackOptions.output
                // both options are optional
                filename: "[name]-[hash].css"
            })
        ],
        module: {
            rules: [
                {
                    test: /\.elm$/,
                    loader: StringReplacePlugin.replace({
                        replacements: [
                            {
                                pattern: /__WEBPACK_CONSTANT_API_BASE_URL__/g,
                                replacement: function (match, p1, offset, string) {
                                   return PROD_API_URL;
                                }
                           },
                           {
                               pattern: /__WEBPACK_CONSTANT_OAUTH_CLIENT_ID__/g,
                               replacement: function(match, p1, offset, string) {
                                   return "Iv1.5f4d835ee23e3947";
                               }
                           },
                           {
                               pattern: /__WEBPACK_CONSTANT_GITHUB_APP_NAME__/g,
                               replacement: function(match, p1, offset, string) {
                                   return "vivadoc"
                               }
                           },
                           {
                               pattern: /__WEBPACK_CONSTANT_REDIRECT_URI__/g,
                               replacement: function(match, p1, offset, string) {
                                   return "https://www.vivadoc.io/oauth_redirect";
                               }
                           }
                       ]
                   })
                },
                {
                    test: /\.elm$/,
                    exclude: [/elm-stuff/, /node_modules/],
                    use: {
                        loader: "elm-webpack-loader",
                        options: {
                            optimize: true
                        }
                    }
                },
                {
                    test: /\.css$/,
                    exclude: [/elm-stuff/, /node_modules/],
                    loaders: [
                        MiniCssExtractPlugin.loader,
                        "css-loader?url=false"
                    ]
                },
                {
                    test: /\.scss$/,
                    exclude: [/elm-stuff/, /node_modules/],
                    loaders: [
                        MiniCssExtractPlugin.loader,
                        "css-loader?url=false",
                        "sass-loader"
                    ]
                }
            ]
        }
    });
}
