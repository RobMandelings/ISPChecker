<script setup lang="ts">

import {Module} from '../assets/js/Structs'
import {PropType} from 'vue'

const props = defineProps({
  moduleData: {
    type: Object as PropType<Module>,
    required: true
  },
})

</script>

<template>

</template>

<style scoped>

</style>