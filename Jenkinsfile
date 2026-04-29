def bash(String script) {
  sh """#!/usr/bin/env bash
set -euxo pipefail
${script}
"""
}

def checkedOut() {
  checkout scm
  bash '''
git rev-parse HEAD
git status --short
'''
}

def safeName(String value) {
  return value.replaceAll(/[^A-Za-z0-9_.-]/, '-')
}

def buildImage(String starBase, String compiler) {
  node {
    checkedOut()
    bash '''
command -v docker
docker --version
docker buildx version
docker buildx use default || true
docker buildx inspect default --bootstrap || docker buildx inspect --bootstrap
'''

    withEnv(["STAR_BASE=${starBase}", "COMPILER=${compiler}"]) {
      bash '''
starenv="${STAR_BASE}-${COMPILER}"
image_tag="${IMAGE_PREFIX}-${starenv}"
image_tar="${ARTIFACT_DIR}/star-sw-${starenv}.tar"

mkdir -p "${ARTIFACT_DIR}"

docker buildx build \
  --progress plain \
  --build-arg "starenv=${STAR_BASE}" \
  --build-arg "compiler=${COMPILER}" \
  --tag "${image_tag}" \
  --output "type=docker,dest=${image_tar}" \
  .
'''

      stash name: "star-sw-${starBase}-${compiler}", includes: "artifacts/star-sw-${starBase}-${compiler}.tar"
    }
  }
}

def runExecutest(String testId, String starBase, String compiler) {
  node {
    unstash "star-sw-${starBase}-${compiler}"
    withEnv([
      "TEST_ID=${testId}",
      "STAR_BASE=${starBase}",
      "COMPILER=${compiler}",
      "DATA_CONTAINER=${safeName("${env.BUILD_TAG}-${starBase}-${compiler}-${testId}")}"
    ]) {
      bash '''
starenv="${STAR_BASE}-${COMPILER}"
image_tag="${IMAGE_PREFIX}-${starenv}"
image_tar="${ARTIFACT_DIR}/star-sw-${starenv}.tar"
data_container="star-test-data-${DATA_CONTAINER}"

docker load --input "${image_tar}"
docker rm -f "${data_container}" >/dev/null 2>&1 || true
trap 'docker rm -f "${data_container}" >/dev/null 2>&1 || true' EXIT
docker run --name "${data_container}" --volume /star "${TEST_DATA_IMAGE}"

TEST_CMD="$(docker run --rm "${image_tag}" tests/executest.py -c "${TEST_ID}")"
docker run --volumes-from "${data_container}" "${image_tag}" \
  sh -c "set -e; MALLOC_CHECK_=3 ${TEST_CMD} 2>&1 | tee log; grep 'Run completed' log"
'''
    }
  }
}

def runRoot5MacroTest(String label, String testId, String compiler, String macroCall, String grepPattern) {
  node {
    unstash "star-sw-root5-${compiler}"
    withEnv([
      "TEST_ID=${testId}",
      "COMPILER=${compiler}",
      "MACRO_CALL=${macroCall}",
      "GREP_PATTERN=${grepPattern}",
      "DATA_CONTAINER=${safeName("${env.BUILD_TAG}-${label}-${compiler}-${testId}")}"
    ]) {
      bash '''
starenv="root5-${COMPILER}"
image_tag="${IMAGE_PREFIX}-${starenv}"
image_tar="${ARTIFACT_DIR}/star-sw-${starenv}.tar"
data_container="star-test-data-${DATA_CONTAINER}"

docker load --input "${image_tar}"
docker rm -f "${data_container}" >/dev/null 2>&1 || true
trap 'docker rm -f "${data_container}" >/dev/null 2>&1 || true' EXIT
docker run --name "${data_container}" --volume /star "${TEST_DATA_IMAGE}"

TEST_FILE="$(docker run --rm "${image_tag}" tests/executest.py "${TEST_ID}" -a fullpath | sed -E 's/\\.(daq|fzd)$/.event.root/')"
TEST_CMD="root4star -b -q -l '${MACRO_CALL}'"
TEST_CMD="${TEST_CMD//\\$TEST_FILE/${TEST_FILE}}"
docker run --volumes-from "${data_container}" "${image_tag}" \
  sh -c "set -e; ${TEST_CMD} 2>&1 | tee log; grep '${GREP_PATTERN}' log"
'''
    }
  }
}

def branchesFor(List values, Closure body) {
  def branches = [:]
  values.each { value ->
    def item = value
    branches[item.toString()] = { body(item) }
  }
  return branches
}

pipeline {
  agent none

  triggers {
    GenericTrigger(
      genericVariables: [
        [key: 'ref', value: '$.ref', defaultValue: ''],
        [key: 'action', value: '$.action', defaultValue: '']
      ],
      causeString: 'GitHub webhook: action=$action ref=$ref',
      regexpFilterText: '$action:$ref',
      regexpFilterExpression: '^(:refs/heads/(main|jenkins-ci-pipeline-webhook)|(opened|synchronize|reopened|ready_for_review):)$',
      printContributedVariables: true,
      printPostContent: false,
      silentResponse: false
    )
  }

  options {
    timestamps()
    skipDefaultCheckout(true)
    disableConcurrentBuilds(abortPrevious: true)
  }

  environment {
    ARTIFACT_DIR = 'artifacts'
    IMAGE_PREFIX = 'ghcr.io/star-bnl/star-sw'
    TEST_DATA_IMAGE = 'ghcr.io/star-bnl/star-test-data:v7'
  }

  stages {
    stage('Build Docker Images') {
      steps {
        script {
          def builds = branchesFor(['root5-gcc485', 'root5-gcc11', 'root6-gcc485', 'root6-gcc11']) { combo ->
            def parts = combo.split('-')
            buildImage(parts[0], parts[1])
          }
          parallel builds
        }
      }
    }

    stage('Test') {
      steps {
        script {
          def testIds = '10,11,22,23,24,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,59,60,76,77,78,90,91,92,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,123,124,125'.split(',') as List
          def combos = []
          testIds.each { id ->
            combos << "${id}-root5-gcc485"
            combos << "${id}-root6-gcc485"
          }

          def tests = branchesFor(combos) { combo ->
            def parts = combo.split('-')
            runExecutest(parts[0], parts[1], parts[2])
          }
          parallel tests
        }
      }
    }

    stage('ROOT5 test doEvents') {
      steps {
        script {
          def tests = branchesFor(['121-gcc485', '121-gcc11', '122-gcc485', '122-gcc11']) { combo ->
            def parts = combo.split('-')
            runRoot5MacroTest(
              'doEvents',
              parts[0],
              parts[1],
              'StRoot/macros/analysis/doEvents.C(100, "$TEST_FILE")',
              '<StIOMaker::Finish> IO:'
            )
          }
          parallel tests
        }
      }
    }

    stage('ROOT5 test find vertex') {
      steps {
        script {
          def tests = branchesFor(['102-gcc485', '102-gcc11', '121-gcc485', '121-gcc11', '122-gcc485', '122-gcc11']) { combo ->
            def parts = combo.split('-')
            runRoot5MacroTest(
              'find-vertex',
              parts[0],
              parts[1],
              'StRoot/macros/analysis/find_vertex.C("$TEST_FILE")',
              '<StIOMaker::Finish> StIO:'
            )
          }
          parallel tests
        }
      }
    }
  }

  post {
    always {
      echo "Build finished: ${currentBuild.currentResult}"
      echo "Branch: ${env.BRANCH_NAME ?: 'unknown'}"
      echo "Pull request: ${env.CHANGE_ID ?: 'none'}"
    }
  }
}
