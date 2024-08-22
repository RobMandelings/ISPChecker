/** @type {import('tailwindcss').Config} */
export default {
    content: [ // Specifies the paths to all files that contain tailwind class names. Required such that tailwind knows which classess are being used and that only the css for used classes is being generated.
        "./src/**/*.{vue,js,ts,jsx,tsx}",
        './src/*.vue',
        './node_modules/preline/preline.js',
    ],
    theme: {
        extend: {},
    },
    plugins: [
        require('preline/plugin')
    ],
}