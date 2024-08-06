<script setup lang="ts">
import Module from './ModuleOverview.vue'

import {ref, onMounted, computed} from 'vue'

import * as Parser from "../assets/js/Parser"
import * as Structs from "../assets/js/Structs"
import axios from "axios";
import ModuleOverview from "./ModuleOverview.vue";
//
// const mod = {
//   "commonFields": {
//     "name": "Verdere optie",
//     "description": "Hellowkes",
//     "courses": [
//       "H04IOA"
//     ],
//     "constraints": [
//       {
//         "tag": "ModuleConstraint",
//         "contents": [
//           "Maximaal 1 studiepunt",
//           {
//             "tag": "MaxSPConstraint",
//             "contents": 1
//           }
//         ]
//       }
//     ],
//     "activator": "\"\u003CModuleActivator\u003E\""
//   },
//   "subModules": []
// }

const mods = ref<Record<string, Structs.Module>>({});

const module = computed(() => mods.value?.["abc"]);

// const module = ref(Parser.parseModule(mod));
const courses = ref<Structs.Course[]>([]);


const loadData = async () => {
  const courseRes = await axios.get('http://localhost:3000/res/courses');
  courses.value = courseRes.data;

  const modsRes = await axios.get('http://localhost:3000/res/mods');
  mods.value = Object.fromEntries(Object.entries(modsRes.data).map(([key, val]) => [key, Parser.parseModule(val)]));
}


onMounted(() => {
  console.log('Hello');

  loadData();
})

defineProps({
  msg: String,
})

const count = ref(0)
const test = ref(module.value);

</script>

<template>
  <div>
    <ModuleOverview v-if="module" :module-data="module" :courses="courses"></ModuleOverview>
  </div>
</template>

<style scoped>
.read-the-docs {
  color: #888;
}
</style>
