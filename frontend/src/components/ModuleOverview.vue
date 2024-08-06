<script setup lang="ts">

import * as Structs from '../assets/js/Structs'
import {PropType} from 'vue'
import {FwbAccordion, FwbAccordionHeader, FwbAccordionPanel, FwbAccordionContent, FwbTooltip} from 'flowbite-vue';
import {FontAwesomeIcon} from "@fortawesome/vue-fontawesome";

const props = defineProps({
  moduleData: {
    type: Object as PropType<Structs.Module>,
    required: true
  },
  courses: {
    type: Object as PropType<Record<string, Structs.Course>>,
    required: true
  },
  checkResult: {
    type: Object,
  }
})

</script>

<template>
  <div v-if="moduleData" class="w-full">
    <fwb-accordion>
      <fwb-accordion-panel>
        <fwb-accordion-header>
          <div class="flex flex-row justify-between">
            <div class="flex flex-row">
              <fwb-tooltip>
                <template #trigger>
                  <font-awesome-icon :icon="['fas', 'question-circle']"/>
                </template>
                <template #content>
                  {{ moduleData.desc }}
                </template>
              </fwb-tooltip>
              <div>{{ moduleData.name }}</div>
            </div>
            <div class="flex flex-row">
              Hello
            </div>
          </div>
        </fwb-accordion-header>
        <fwb-accordion-content>
          <div class="divide-y divide-dashed">
            <div class="flex flex-col items-start p-1">
              <div v-for="mConstraint in moduleData.moduleConstraints">
                {{ mConstraint.description }}
              </div>
            </div>
            <div class="flex flex-col items-start p-1">
              <div v-for="courseCode in moduleData.courseCodes" class="w-full">
                <div class="flex flex-row justify-between">
                  <div class="flex flex-row">
                    <fwb-tooltip v-if="courses[courseCode].description">
                      <template #trigger>
                        <font-awesome-icon :icon="['fas', 'question-circle']"/>
                      </template>
                      <template #content>
                        {{ courses[courseCode].description }}
                      </template>
                    </fwb-tooltip>
                    <div class="flex flex-row space-x-3">
                      <div class="font-bold">{{ courses[courseCode].code }}</div>
                      <div>{{ courses[courseCode].name }}</div>
                    </div>
                  </div>
                  <div class="flex flex-row space-x-2">
                    <div>{{ courses[courseCode].studyPoints }}SP</div>
                    <div>
                      <div v-if="courses[courseCode].semester == 1">1</div>
                      <div v-if="courses[courseCode].semester == 2">2</div>
                      <div v-if="courses[courseCode].semester == 2">3</div>
                    </div>
                  </div>
                </div>
              </div>
            </div>
            <div class="flex flex-col items-start p-1">
              <div v-for="subModule in moduleData.subModules" class="w-full">
                <ModuleOverview :module-data="subModule" :courses="courses"></ModuleOverview>
              </div>
            </div>
          </div>
        </fwb-accordion-content>
      </fwb-accordion-panel>
    </fwb-accordion>
  </div>


</template>

<style scoped>

</style>