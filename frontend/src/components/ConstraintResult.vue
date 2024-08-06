<script setup lang="ts">

import * as Structs from "../assets/js/Structs";
import {PropType} from "vue";

defineProps({
  constraintResult: {
    type: Object as PropType<Structs.ConstraintResult>
  }
})

</script>

<template>
  <ul v-if="constraintResult.failed">
    <li>{{ constraintResult.errorMsg }}
      <ConstraintResult v-for="subRes in constraintResult.subResults" :constraint-result="subRes"></ConstraintResult>
    </li>
  </ul>
</template>

<style scoped>

</style>