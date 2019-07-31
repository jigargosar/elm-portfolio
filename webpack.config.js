const path = require('path')
const HtmlWebpackPlugin = require('html-webpack-plugin')
const { CleanWebpackPlugin } = require('clean-webpack-plugin')


const NODE_ENV = process.env.NODE_ENV
const isProduction = NODE_ENV === 'production'

console.log('NODE_ENV', NODE_ENV)

// https://webpack.js.org/configuration/
module.exports = {
  entry: './src/index.js',
  output: {
    publicPath: '/',
  },
  resolve: {
    extensions: ['.js', '.ts', '.tsx', '.jsx', '.elm'],
  },
  module: {
    rules: [
      { test: /\.css$/, loader: ['style-loader', 'css-loader'] },
      {
        test: /\.elm$/,
        use: [
          // 'elm-hot-webpack-loader',
          {
            loader: 'elm-webpack-loader',
            options: {
              // report: 'json'
            },
          },
        ],
      },
      // {
      //   test: /\.[jt]sx?$/,
      //   include: path.resolve('src'),
      //   use: [
      //     {
      //       loader: 'ts-loader',
      //       options: {
      //         // to enable ts-loader work with HMR
      //         // transpileOnly: true,
      //       },
      //     },
      //   ],
      // },
    ],
  },
  plugins: [
    new CleanWebpackPlugin(),
    new HtmlWebpackPlugin({ template: './src/index.html' }),
  ],
  // https://webpack.js.org/configuration/stats/
  // stats: 'errors-warnings',
  stats: {
    children: false,
    modules: false,
  },
  // devtool: isProduction ? 'source-map' : 'eval-source-map',
  devtool: isProduction ? 'source-map' : false,
  // https://webpack.js.org/configuration/dev-server/
  devServer: {
    after: (app, server) => {
      setInterval(() => {
        server.sockWrite(server.sockets, 'content-changed')
      }, 5000)
    },
    proxy: {
      '/api': {
        target: 'http://localhost:3000',
        pathRewrite: { '^/api': '' },
      },
    },
    historyApiFallback: true,
    overlay: {
      warnings: true,
      errors: true,
    },
    hot: true,
    // hotOnly: true,
    contentBase: false,
  },
}
