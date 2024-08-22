import {createApp} from 'vue'
import App from './App.vue'
import {library} from "@fortawesome/fontawesome-svg-core";
import {fas} from "@fortawesome/free-solid-svg-icons";
import {FontAwesomeIcon} from "@fortawesome/vue-fontawesome";

import './style.css'
import '../node_modules/flowbite-vue/dist/index.css'
import './index.css'

library.add(fas)

createApp(App).component("font-awesome-icon", FontAwesomeIcon).mount('#app')
