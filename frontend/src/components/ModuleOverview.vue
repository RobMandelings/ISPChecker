<script setup lang="ts">

import * as Structs from '../assets/js/Structs'
import {onMounted, PropType} from 'vue'
import {FontAwesomeIcon} from "@fortawesome/vue-fontawesome";
import ConstraintResult from "./ConstraintResult.vue";
import {v4 as uuid} from 'uuid'

const id = uuid();

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

const successColor = 'green-500';
const failColor = 'red-500';

import {initFlowbite} from 'flowbite'
import ModuleAccordion from "./ModuleAccordion.vue";

import {FwbTooltip} from 'flowbite-vue';
import {RES_STATUS} from "../assets/js/Structs";

// initialize components based on data attribute selectors
onMounted(() => {
  initFlowbite();
})

</script>

<template>
  <div>
    <div v-if="moduleData && checkResult">
      <div :id="`accordion-open-${id}`" data-accordion="open">
        <ModuleAccordion :module-data="moduleData" :checkResult="checkResult">
          <div class="divide-y divide-dashed">
            <div class="flex flex-col items-start p-1">
              <div v-for="(mConstraint, i) in moduleData.moduleConstraints">
                <div class="text-start" v-if="checkResult">
                  <template v-if="checkResult.status === RES_STATUS.FAILED">
                    <template v-if="checkResult.constraintResults[i].failed">
                      <fwb-tooltip>
                        <template #trigger>
                          <div :class="`text-${failColor}`"> {{ mConstraint.description }}</div>
                        </template>
                        <template #content>
                          <ConstraintResult :constraint-result="checkResult.constraintResults[i]"></ConstraintResult>
                        </template>
                      </fwb-tooltip>
                    </template>
                    <template v-else>
                      <div :class="`text-${successColor}`"> {{ mConstraint.description }}</div>
                    </template>
                  </template>
                  <template v-else-if="checkResult.status === RES_STATUS.SUCCESS">
                    <div :class="`text-${successColor}`"> {{ mConstraint.description }}</div>
                  </template>
                  <template v-else>
                    <div> {{ mConstraint.description }}</div>
                  </template>
                </div>
                <div v-else>
                  {{ mConstraint.description }}
                </div>
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
                      <div v-else-if="courses[courseCode].semester == 2">2</div>
                      <div v-else-if="courses[courseCode].semester == 3">3</div>
                    </div>
                  </div>
                </div>
              </div>
            </div>
            <div class="flex flex-col items-start p-1 space-y-1">
              <div v-for="(subModule, i) in moduleData.subModules" class="w-full">
                <ModuleOverview :module-data="subModule" :courses="courses"
                                :checkResult="!checkResult ? checkResult : (checkResult.status === RES_STATUS.FAILED ? checkResult.subModuleResults[i] : checkResult)"></ModuleOverview>
              </div>
            </div>
          </div>
        </ModuleAccordion>
      </div>
    </div>
  </div>
</template>

<!--<template>-->
<!--  <div v-if="moduleData" class="w-full">-->
<!--    <fwb-accordion>-->
<!--      <fwb-accordion-panel>-->
<!--        <fwb-accordion-header>-->
<!--          <div>-->
<!--            <div class="flex flex-row justify-between">-->
<!--              <div class="flex flex-row">-->
<!--                <fwb-tooltip>-->
<!--                  <template #trigger>-->
<!--                    <font-awesome-icon :icon="['fas', 'question-circle']"/>-->
<!--                  </template>-->
<!--                  <template #content>-->
<!--                    {{ moduleData.desc }}-->
<!--                  </template>-->
<!--                </fwb-tooltip>-->
<!--                <div v-if="checkResult">-->
<!--                  <div :class="checkResult.failed ? `text-${failColor}` : `text-${successColor}`"> {{-->
<!--                      moduleData.name-->
<!--                    }}-->
<!--                  </div>-->
<!--                </div>-->
<!--                <div v-else>-->
<!--                  {{ moduleData.name }}-->
<!--                </div>-->
<!--              </div>-->
<!--              <div class="flex flex-row">-->
<!--                Hello-->
<!--              </div>-->
<!--            </div>-->
<!--          </div>-->
<!--        </fwb-accordion-header>-->
<!--        <fwb-accordion-content>-->
<!--          <div class="divide-y divide-dashed">-->
<!--            <div class="flex flex-col items-start p-1">-->
<!--              <div v-for="(mConstraint, i) in moduleData.moduleConstraints">-->
<!--                <div v-if="checkResult">-->
<!--                  <template v-if="checkResult.failed && checkResult.constraintResults[i].failed">-->
<!--                    <fwb-tooltip>-->
<!--                      <template #trigger>-->
<!--                        <div :class="`text-${failColor}`"> {{ mConstraint.description }}</div>-->
<!--                      </template>-->
<!--                      <template #content>-->
<!--                        <ConstraintResult :constraint-result="checkResult.constraintResults[i]"></ConstraintResult>-->
<!--                      </template>-->
<!--                    </fwb-tooltip>-->
<!--                  </template>-->
<!--                  <template v-else>-->
<!--                    <div :class="`text-${successColor}`"> {{ mConstraint.description }}</div>-->
<!--                  </template>-->
<!--                </div>-->
<!--                <div v-else>-->
<!--                  {{ mConstraint.description }}-->
<!--                </div>-->
<!--              </div>-->
<!--            </div>-->
<!--            <div class="flex flex-col items-start p-1">-->
<!--              <div v-for="courseCode in moduleData.courseCodes" class="w-full">-->
<!--                <div class="flex flex-row justify-between">-->
<!--                  <div class="flex flex-row">-->
<!--                    <fwb-tooltip v-if="courses[courseCode].description">-->
<!--                      <template #trigger>-->
<!--                        <font-awesome-icon :icon="['fas', 'question-circle']"/>-->
<!--                      </template>-->
<!--                      <template #content>-->
<!--                        {{ courses[courseCode].description }}-->
<!--                      </template>-->
<!--                    </fwb-tooltip>-->
<!--                    <div class="flex flex-row space-x-3">-->
<!--                      <div class="font-bold">{{ courses[courseCode].code }}</div>-->
<!--                      <div>{{ courses[courseCode].name }}</div>-->
<!--                    </div>-->
<!--                  </div>-->
<!--                  <div class="flex flex-row space-x-2">-->
<!--                    <div>{{ courses[courseCode].studyPoints }}SP</div>-->
<!--                    <div>-->
<!--                      <div v-if="courses[courseCode].semester == 1">1</div>-->
<!--                      <div v-else-if="courses[courseCode].semester == 2">2</div>-->
<!--                      <div v-else-if="courses[courseCode].semester == 3">3</div>-->
<!--                    </div>-->
<!--                  </div>-->
<!--                </div>-->
<!--              </div>-->
<!--            </div>-->
<!--            <div class="flex flex-col items-start p-1">-->
<!--              <div v-for="(subModule, i) in moduleData.subModules" class="w-full">-->
<!--                <ModuleOverview :module-data="subModule" :courses="courses"-->
<!--                                :checkResult="!checkResult ? checkResult : (checkResult.failed ? checkResult.subModuleResults[i] : checkResult)"></ModuleOverview>-->
<!--              </div>-->
<!--            </div>-->
<!--          </div>-->
<!--        </fwb-accordion-content>-->
<!--      </fwb-accordion-panel>-->
<!--    </fwb-accordion>-->
<!--  </div>-->


<!--</template>-->

<style scoped>

</style>