<script setup lang="ts">

import {ISP} from '../assets/js/Structs';
import {PropType} from "vue";

const props = defineProps({
  isp: {
    type: Object as PropType<ISP>,
    required: true
  }
})

</script>

<template>
  <div class="border text-left">
    <div class="text-3xl">ISP</div>
    <div class="text-2xl">Course selection</div>
    <div class="text-xl font-bold">Passed</div>
    <div v-if="isp.courseSelection.passed.length" class="border overflow-auto text-green-500"
         style="max-height: 100px;">
      <div v-for="courseCode in isp.courseSelection.passed">{{ courseCode }}</div>
    </div>
    <div class="text-xl font-bold">Planned</div>
    <div v-if="isp.courseSelection.planned.length" class="border overflow-auto"
         style="max-height: 100px;">
      <template v-for="(semCourses, sem) in isp.courseSelection.planned">
        <div class="text-lg">Semester {{ sem }}</div>
        <div class="text-yellow-300">
          <div v-for="courseCode in semCourses">{{ courseCode }}</div>
        </div>
      </template>
    </div>

  </div>
</template>

<style scoped>

</style>