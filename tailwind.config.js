module.exports = {
  darkMode: 'class',
  mode: 'jit',
  content: [
    './*.html',
    './src/**/*.purs',
  ],
  theme: {
    extend: {
      fontFamily: {
        AlfaSlabOne: ['Alfa Slab One', 'cursive'],
        Inconsolata: ['Inconsolata', 'monospace']
      }
    },
  },
  plugins: [],
}
