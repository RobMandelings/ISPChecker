import {createApp} from 'vue'
import './style.css'
import App from './App.vue'
import 'tailwindcss/tailwind.css';
import {library} from "@fortawesome/fontawesome-svg-core";
import {fas} from "@fortawesome/free-solid-svg-icons";
import {FontAwesomeIcon} from "@fortawesome/vue-fontawesome";

library.add(fas)

import './index.css'

createApp(App).component("font-awesome-icon", FontAwesomeIcon).mount('#app')
