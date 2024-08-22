/** @type {import('tailwindcss').Config} */
import FlowbitePlugin from 'flowbite/plugin'

export default {
    content: [ // Specifies the paths to all files that contain tailwind class names. Required such that tailwind knows which classess are being used and that only the css for used classes is being generated.
        "./src/**/*.{vue,js,ts,jsx,tsx}",
        './src/*.vue',
        './node_modules/flowbite-vue/**/*.{js,jsx,ts,tsx,vue}',
        "./node_modules/flowbite/**/*.js",
        "./index.html"
    ],
    theme: {
        extend: {},
    },
    plugins: [FlowbitePlugin],
}