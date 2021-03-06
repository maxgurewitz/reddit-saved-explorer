const path = require('path')
const webpack = require('webpack');
const HtmlWebpackPlugin = require('html-webpack-plugin')
const merge = require('webpack-merge')
const CleanWebpackPlugin = require('clean-webpack-plugin')
const CopyWebpackPlugin = require('copy-webpack-plugin')
const MiniCssExtractPlugin = require('mini-css-extract-plugin')
const WebpackBuildNotifierPlugin = require('webpack-build-notifier')
const OptimizeCssAssetsPlugin = require('optimize-css-assets-webpack-plugin')
const autoprefixer = require('autoprefixer')
const CompressionPlugin = require('compression-webpack-plugin')
const UglifyJsPlugin = require('uglifyjs-webpack-plugin');
const { OpenBrowserPlugin } = require('./webpack-util')

const PORT = process.env.PORT || 3000
const OUTPUT_DIR = 'docs';

const development = {
  mode: 'development',
  watch: true,
  devtool: 'cheap-module-eval-source-map',
  plugins: [
    // Notify on build errors
    new WebpackBuildNotifierPlugin({
      suppressSuccess: 'always',
    }),

    new OpenBrowserPlugin(PORT),
  ],

  devServer: {
    port: PORT,
    overlay: true,
    inline: true,
    historyApiFallback: true,
  },
}

const production = {
  mode: 'production',
  output: {
    path: path.resolve(__dirname, OUTPUT_DIR),
    filename: '[name]-[hash].js',
  },
  optimization: {
    splitChunks: {
      cacheGroups: {
        default: false,
        elm: {
          test: /[\\/]Main.elm/,
          name: 'elm',
          chunks: 'all'
        }
      }
    },

    // https://github.com/elm-community/elm-webpack-loader/issues/140
    // https://github.com/mishoo/UglifyJS2/issues/2438
    minimizer: [
      new UglifyJsPlugin({ exclude: 'elm', sourceMap: true, parallel: true }),
      new UglifyJsPlugin({
          include: 'elm',
          parallel: true,
          sourceMap: false,
          uglifyOptions: {
              compress: {
                  pure_funcs: [
                      'F2',
                      'F3',
                      'F4',
                      'F5',
                      'F6',
                      'F7',
                      'F8',
                      'F9',
                      'A2',
                      'A3',
                      'A4',
                      'A5',
                      'A6',
                      'A7',
                      'A8',
                      'A9'
                  ],
                  pure_getters: true,
                  keep_fargs: false,
                  unsafe_comps: true,
                  unsafe: true
              }
          }
      }),
    ]
  },
  plugins: [
    new webpack.ExtendedAPIPlugin(),
    new OptimizeCssAssetsPlugin(),
    // gzip's assets
    new CompressionPlugin({
      test: /^(elm|main)-.+\.(js|css)$/,
      cache: true,
      threshold: 1024,
    }),
  ],
}

const common = (env, argv) => {
  const { mode } = argv
  return {
    output: {
      publicPath: mode === 'development' ? '' : '/reddit-saved-explorer'
    },
    module: {
      rules: [
        {
          test: /\.js$/,
          exclude: /node_modules\/(?!snoowrap)/,
          use: {
            loader: 'babel-loader',
            options: {
              presets: ['@babel/preset-env']
            }
          }
        },
        {
          test: /\.elm$/,
          exclude: [/elm-stuff/, /node_modules/],
          loader: 'elm-webpack-loader',
          options: {
            optimize: mode != 'development',
            // Shows the model history overlay
            debug: mode === 'development',
          },
        },
        {
          test: /\.(woff|woff2|eot|ttf|otf|svg)$/,
          include: [
            path.resolve(__dirname, 'public'),
            path.resolve(__dirname, 'node_modules', 'bootstrap-sass', 'assets', 'fonts', 'bootstrap')
          ],
          use: ['file-loader'],
        },
        {
          test: /\.(css|scss|sass)$/,
          include: [
            path.resolve(__dirname, 'src'),
          ],
          use: [
            mode === 'production'
              ? MiniCssExtractPlugin.loader
              : 'style-loader',
            'css-loader',
            {
              loader: 'sass-loader',
              options: {
                includePaths: []
              }
            },
            {
              loader: 'postcss-loader',
              options: {
                plugins: [autoprefixer()],
              },
            },
          ],
        },
      ],
    },

    plugins: [
      new webpack.EnvironmentPlugin({
        clientId: mode === 'development' ?
          '30REJ3DOhCSiBQ' :
          'O0SO1Y-rWRd3hQ',

        redirectUri: mode === 'development' ?
          'http://localhost:3000' :
          'https://maxgurewitz.github.io/reddit-saved-explorer/'
      }),

      new CleanWebpackPlugin([OUTPUT_DIR]),

      new MiniCssExtractPlugin({
        filename:
          mode === 'development' ? '[name].css' : '[name]-[contenthash].css',
      }),

      new HtmlWebpackPlugin({
        title: 'reddit-saved-explorer',
        template: 'public/index.html',
      }),

      new CopyWebpackPlugin([
        {
          from: 'public/img',
          to: 'img',
        },
        {
          from: 'public/favicon.ico',
        },
      ]),
    ],
  }
}

module.exports = (env, argv) => {
  const { mode } = argv;
  return merge(
    common(env, argv),
    mode === 'development' ? development : production
  );
}
