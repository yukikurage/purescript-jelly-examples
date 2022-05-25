module.exports = {
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
