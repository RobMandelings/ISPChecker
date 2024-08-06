<script setup lang="ts">
import ModuleOverview from './ModuleOverview.vue'

import {computed, onMounted, ref} from 'vue'

import * as Parser from "../assets/js/Parser"
import * as Structs from "../assets/js/Structs"
import axios from "axios";
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

const constraintResult = ref<Structs.ModuleConstraintResult>({})

// const module = ref(Parser.parseModule(mod));
const courses = ref<Record<string, Structs.Course>>({});

const loadData = async () => {
  const courseRes = await axios.get('http://localhost:3000/res/courses');
  courses.value = Object.fromEntries(Object.entries(courseRes.data).map(([key, val]) => [key, Parser.parseCourse(val)]));

  const modsRes = await axios.get('http://localhost:3000/res/mods');
  mods.value = Object.fromEntries(Object.entries(modsRes.data).map(([key, val]) => [key, Parser.parseModule(val)]));

  const runRes = await axios.get('http://localhost:3000/run');
  const runJson = runRes.data;
  constraintResult.value = Parser.parseModuleConstraintResult(runJson);
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
  <div style="width: 800px">
    <ModuleOverview v-if="module" :module-data="module" :courses="courses"
                    :check-result="constraintResult"></ModuleOverview>
  </div>
</template>

<style scoped>
.read-the-docs {
  color: #888;
}
</style>
