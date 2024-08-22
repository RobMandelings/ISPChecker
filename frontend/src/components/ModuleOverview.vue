<script setup lang="ts">

import * as Structs from '../assets/js/Structs'
import {PropType} from 'vue'
import {FontAwesomeIcon} from "@fortawesome/vue-fontawesome";
import ConstraintResult from "./ConstraintResult.vue";

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

</script>

<template>
  <div>
    <button id="dropdownDefaultButton" data-dropdown-toggle="dropdown"
            class="text-white bg-blue-700 hover:bg-blue-800 focus:ring-4 focus:outline-none focus:ring-blue-300 font-medium rounded-lg text-sm px-5 py-2.5 text-center inline-flex items-center dark:bg-blue-600 dark:hover:bg-blue-700 dark:focus:ring-blue-800"
            type="button">Dropdown button
      <svg class="w-2.5 h-2.5 ms-3" aria-hidden="true" xmlns="http://www.w3.org/2000/svg" fill="none"
           viewBox="0 0 10 6">
        <path stroke="currentColor" stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="m1 1 4 4 4-4"/>
      </svg>
    </button>

    <!-- Dropdown menu -->
    <div id="dropdown" class="z-10 hidden bg-white divide-y divide-gray-100 rounded-lg shadow w-44 dark:bg-gray-700">
      <ul class="py-2 text-sm text-gray-700 dark:text-gray-200" aria-labelledby="dropdownDefaultButton">
        <li>
          <a href="#"
             class="block px-4 py-2 hover:bg-gray-100 dark:hover:bg-gray-600 dark:hover:text-white">Dashboard</a>
        </li>
        <li>
          <a href="#"
             class="block px-4 py-2 hover:bg-gray-100 dark:hover:bg-gray-600 dark:hover:text-white">Settings</a>
        </li>
        <li>
          <a href="#"
             class="block px-4 py-2 hover:bg-gray-100 dark:hover:bg-gray-600 dark:hover:text-white">Earnings</a>
        </li>
        <li>
          <a href="#" class="block px-4 py-2 hover:bg-gray-100 dark:hover:bg-gray-600 dark:hover:text-white">Sign
            out</a>
        </li>
      </ul>
    </div>
  </div>
  <!--  <div id="accordion-open" data-accordion="open">-->
  <!--    <h2 id="accordion-open-heading-1">-->
  <!--      <button type="button"-->
  <!--              class="flex items-center justify-between w-full p-5 font-medium rtl:text-right text-gray-500 border border-b-0 border-gray-200 rounded-t-xl focus:ring-4 focus:ring-gray-200 dark:focus:ring-gray-800 dark:border-gray-700 dark:text-gray-400 hover:bg-gray-100 dark:hover:bg-gray-800 gap-3"-->
  <!--              data-accordion-target="#accordion-open-body-1" aria-expanded="true" aria-controls="accordion-open-body-1">-->
  <!--        <span class="flex items-center"><svg class="w-5 h-5 me-2 shrink-0" fill="currentColor" viewBox="0 0 20 20"-->
  <!--                                             xmlns="http://www.w3.org/2000/svg"><path fill-rule="evenodd"-->
  <!--                                                                                      d="M18 10a8 8 0 11-16 0 8 8 0 0116 0zm-8-3a1 1 0 00-.867.5 1 1 0 11-1.731-1A3 3 0 0113 8a3.001 3.001 0 01-2 2.83V11a1 1 0 11-2 0v-1a1 1 0 011-1 1 1 0 100-2zm0 8a1 1 0 100-2 1 1 0 000 2z"-->
  <!--                                                                                      clip-rule="evenodd"></path></svg> What is Flowbite?</span>-->
  <!--        <svg data-accordion-icon class="w-3 h-3 rotate-180 shrink-0" aria-hidden="true"-->
  <!--             xmlns="http://www.w3.org/2000/svg" fill="none" viewBox="0 0 10 6">-->
  <!--          <path stroke="currentColor" stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M9 5 5 1 1 5"/>-->
  <!--        </svg>-->
  <!--      </button>-->
  <!--    </h2>-->
  <!--    <div id="accordion-open-body-1" class="hidden" aria-labelledby="accordion-open-heading-1">-->
  <!--      <div class="p-5 border border-b-0 border-gray-200 dark:border-gray-700 dark:bg-gray-900">-->
  <!--        <p class="mb-2 text-gray-500 dark:text-gray-400">Flowbite is an open-source library of interactive components-->
  <!--          built on top of Tailwind CSS including buttons, dropdowns, modals, navbars, and more.</p>-->
  <!--        <p class="text-gray-500 dark:text-gray-400">Check out this guide to learn how to <a-->
  <!--            href="/docs/getting-started/introduction/" class="text-blue-600 dark:text-blue-500 hover:underline">get-->
  <!--          started</a> and start developing websites even faster with components on top of Tailwind CSS.</p>-->
  <!--      </div>-->
  <!--    </div>-->
  <!--    <h2 id="accordion-open-heading-2">-->
  <!--      <button type="button"-->
  <!--              class="flex items-center justify-between w-full p-5 font-medium rtl:text-right text-gray-500 border border-b-0 border-gray-200 focus:ring-4 focus:ring-gray-200 dark:focus:ring-gray-800 dark:border-gray-700 dark:text-gray-400 hover:bg-gray-100 dark:hover:bg-gray-800 gap-3"-->
  <!--              data-accordion-target="#accordion-open-body-2" aria-expanded="false"-->
  <!--              aria-controls="accordion-open-body-2">-->
  <!--        <span class="flex items-center"><svg class="w-5 h-5 me-2 shrink-0" fill="currentColor" viewBox="0 0 20 20"-->
  <!--                                             xmlns="http://www.w3.org/2000/svg"><path fill-rule="evenodd"-->
  <!--                                                                                      d="M18 10a8 8 0 11-16 0 8 8 0 0116 0zm-8-3a1 1 0 00-.867.5 1 1 0 11-1.731-1A3 3 0 0113 8a3.001 3.001 0 01-2 2.83V11a1 1 0 11-2 0v-1a1 1 0 011-1 1 1 0 100-2zm0 8a1 1 0 100-2 1 1 0 000 2z"-->
  <!--                                                                                      clip-rule="evenodd"></path></svg>Is there a Figma file available?</span>-->
  <!--        <svg data-accordion-icon class="w-3 h-3 rotate-180 shrink-0" aria-hidden="true"-->
  <!--             xmlns="http://www.w3.org/2000/svg" fill="none" viewBox="0 0 10 6">-->
  <!--          <path stroke="currentColor" stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M9 5 5 1 1 5"/>-->
  <!--        </svg>-->
  <!--      </button>-->
  <!--    </h2>-->
  <!--    <div id="accordion-open-body-2" class="hidden" aria-labelledby="accordion-open-heading-2">-->
  <!--      <div class="p-5 border border-b-0 border-gray-200 dark:border-gray-700">-->
  <!--        <p class="mb-2 text-gray-500 dark:text-gray-400">Flowbite is first conceptualized and designed using the Figma-->
  <!--          software so everything you see in the library has a design equivalent in our Figma file.</p>-->
  <!--        <p class="text-gray-500 dark:text-gray-400">Check out the <a href="https://flowbite.com/figma/"-->
  <!--                                                                     class="text-blue-600 dark:text-blue-500 hover:underline">Figma-->
  <!--          design system</a> based on the utility classes from Tailwind CSS and components from Flowbite.</p>-->
  <!--      </div>-->
  <!--    </div>-->
  <!--    <h2 id="accordion-open-heading-3">-->
  <!--      <button type="button"-->
  <!--              class="flex items-center justify-between w-full p-5 font-medium rtl:text-right text-gray-500 border border-gray-200 focus:ring-4 focus:ring-gray-200 dark:focus:ring-gray-800 dark:border-gray-700 dark:text-gray-400 hover:bg-gray-100 dark:hover:bg-gray-800 gap-3"-->
  <!--              data-accordion-target="#accordion-open-body-3" aria-expanded="false"-->
  <!--              aria-controls="accordion-open-body-3">-->
  <!--        <span class="flex items-center"><svg class="w-5 h-5 me-2 shrink-0" fill="currentColor" viewBox="0 0 20 20"-->
  <!--                                             xmlns="http://www.w3.org/2000/svg"><path fill-rule="evenodd"-->
  <!--                                                                                      d="M18 10a8 8 0 11-16 0 8 8 0 0116 0zm-8-3a1 1 0 00-.867.5 1 1 0 11-1.731-1A3 3 0 0113 8a3.001 3.001 0 01-2 2.83V11a1 1 0 11-2 0v-1a1 1 0 011-1 1 1 0 100-2zm0 8a1 1 0 100-2 1 1 0 000 2z"-->
  <!--                                                                                      clip-rule="evenodd"></path></svg> What are the differences between Flowbite and Tailwind UI?</span>-->
  <!--        <svg data-accordion-icon class="w-3 h-3 rotate-180 shrink-0" aria-hidden="true"-->
  <!--             xmlns="http://www.w3.org/2000/svg" fill="none" viewBox="0 0 10 6">-->
  <!--          <path stroke="currentColor" stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M9 5 5 1 1 5"/>-->
  <!--        </svg>-->
  <!--      </button>-->
  <!--    </h2>-->
  <!--    <div id="accordion-open-body-3" class="hidden" aria-labelledby="accordion-open-heading-3">-->
  <!--      <div class="p-5 border border-t-0 border-gray-200 dark:border-gray-700">-->
  <!--        <p class="mb-2 text-gray-500 dark:text-gray-400">The main difference is that the core components from Flowbite-->
  <!--          are open source under the MIT license, whereas Tailwind UI is a paid product. Another difference is that-->
  <!--          Flowbite relies on smaller and standalone components, whereas Tailwind UI offers sections of pages.</p>-->
  <!--        <p class="mb-2 text-gray-500 dark:text-gray-400">However, we actually recommend using both Flowbite, Flowbite-->
  <!--          Pro, and even Tailwind UI as there is no technical reason stopping you from using the best of two worlds.</p>-->
  <!--        <p class="mb-2 text-gray-500 dark:text-gray-400">Learn more about these technologies:</p>-->
  <!--        <ul class="ps-5 text-gray-500 list-disc dark:text-gray-400">-->
  <!--          <li><a href="https://flowbite.com/pro/" class="text-blue-600 dark:text-blue-500 hover:underline">Flowbite-->
  <!--            Pro</a></li>-->
  <!--          <li><a href="https://tailwindui.com/" rel="nofollow" class="text-blue-600 dark:text-blue-500 hover:underline">Tailwind-->
  <!--            UI</a></li>-->
  <!--        </ul>-->
  <!--      </div>-->
  <!--    </div>-->
  <!--  </div>-->
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