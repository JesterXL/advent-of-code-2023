module.exports = {
    content: [ // change "content" to "purge" in v2
      './src/index.html',
      './src/elm.js',
      './node_modules/flowbite/**/*.js'
    ],
    darkMode: 'class',
    theme: {
      extend: {
       
      },
    },
    variants: {
      extend: {},
    },
    plugins: [
      require('flowbite/plugin')
    ],
  }
  