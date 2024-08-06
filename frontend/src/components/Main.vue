<script setup lang="ts">
import Module from './ModuleOverview.vue'

import {ref, onMounted} from 'vue'

import * as Parser from "../assets/js/Parser"
import * as Structs from "../assets/js/Structs"
import axios from "axios";

const mod = {
  "commonFields": {
    "name": "Verdere optie",
    "description": "Hellowkes",
    "courses": [
      "H04IOA"
    ],
    "constraints": [
      {
        "tag": "ModuleConstraint",
        "contents": [
          "Maximaal 1 studiepunt",
          {
            "tag": "MaxSPConstraint",
            "contents": 1
          }
        ]
      }
    ],
    "activator": "\"\u003CModuleActivator\u003E\""
  },
  "subModules": []
}

const module = ref(Parser.parseModule(mod));
const courses = ref<Structs.Course[]>([]);

const loadData = async () => {
  const res = await axios.get('http://localhost:3000/res/courses');
  courses.value = res.data;
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
    <Module :module-data="module"></Module>
  </div>
</template>

<style scoped>
.read-the-docs {
  color: #888;
}
</style>
