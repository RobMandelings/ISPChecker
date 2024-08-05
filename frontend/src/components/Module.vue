<script setup lang="ts">

import {Module} from '../assets/js/Structs'
import {PropType} from 'vue'
import {onMounted} from 'vue'
import {initFlowbite} from 'flowbite'
import {FwbDropdown, FwbListGroup, FwbListGroupItem} from 'flowbite-vue'

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
    {{ moduleData }}
    <fwb-dropdown text="Click me" placement="top">
      <fwb-list-group>
        <fwb-list-group-item>Item #1</fwb-list-group-item>
        <fwb-list-group-item>Item #2</fwb-list-group-item>
        <fwb-list-group-item>Item #3</fwb-list-group-item>
      </fwb-list-group>
    </fwb-dropdown>

  </div>


</template>

<style scoped>

</style>