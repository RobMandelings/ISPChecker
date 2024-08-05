<script setup lang="ts">

import {Module} from '../assets/js/Structs'
import {PropType} from 'vue'
import {onMounted} from 'vue'
import {initFlowbite} from 'flowbite'
import {FwbAccordion, FwbAccordionHeader, FwbAccordionPanel, FwbAccordionContent, FwbTooltip} from 'flowbite-vue';

const props = defineProps({
  moduleData: {
    type: Object as PropType<Module>,
    required: true
  },
})

onMounted(() => {
  initFlowbite();
})

</script>

<template>
  <div style="width: 600px;">
    <fwb-accordion style="width: 100%">
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
              <div v-for="courseCode in moduleData.courseCodes">
                {{ courseCode }}
              </div>
            </div>
            <div class="flex flex-col items-start p-1">
              <div v-for="subModule in moduleData.subModules">
                <module :module-data="subModule"></module>
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