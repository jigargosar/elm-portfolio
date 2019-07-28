const path = require('path')
const HtmlWebpackPlugin = require('html-webpack-plugin')
const { CleanWebpackPlugin } = require('clean-webpack-plugin')

// https://webpack.js.org/configuration/
module.exports = function(env = {}, argv) {
  console.log('env:', env)
  const isProduction = argv.mode === 'production'
  return {
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
            'elm-hot-webpack-loader',
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
}
