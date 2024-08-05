<script setup lang="ts">

import {Module} from '../assets/js/Structs'
import {PropType} from 'vue'
import {onMounted} from 'vue'
import {initFlowbite} from 'flowbite'
import {FwbAccordion, FwbAccordionHeader, FwbAccordionPanel, FwbAccordionContent} from 'flowbite-vue';

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
  <div>
    <fwb-accordion>
      <fwb-accordion-panel>
        <fwb-accordion-header>header</fwb-accordion-header>
        <fwb-accordion-content>Heloo</fwb-accordion-content>
      </fwb-accordion-panel>
    </fwb-accordion>
  </div>


</template>

<style scoped>

</style>