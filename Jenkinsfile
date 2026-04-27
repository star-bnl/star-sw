pipeline {
  agent none

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
      matrix {
        agent any

        axes {
          axis {
            name 'STAR_BASE'
            values 'root5', 'root6'
          }
          axis {
            name 'COMPILER'
            values 'gcc485', 'gcc11'
          }
        }

        stages {
          stage('Checkout') {
            steps {
              checkout scm
              sh '''#!/usr/bin/env bash
                set -euxo pipefail
                git rev-parse HEAD
                git status --short
              '''
            }
          }

          stage('Prepare Docker Buildx') {
            steps {
              sh '''#!/usr/bin/env bash
                set -euxo pipefail
                command -v docker
                docker --version
                docker buildx version
                docker buildx use default || true
                docker buildx inspect default --bootstrap || docker buildx inspect --bootstrap
              '''
            }
          }

          stage('Build With Docker') {
            steps {
              sh '''#!/usr/bin/env bash
                set -euxo pipefail
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
            }
          }

          stage('Save Built Image For Test Jobs') {
            steps {
              archiveArtifacts artifacts: "artifacts/star-sw-${STAR_BASE}-${COMPILER}.tar", fingerprint: true
              stash name: "star-sw-${STAR_BASE}-${COMPILER}", includes: "artifacts/star-sw-${STAR_BASE}-${COMPILER}.tar"
            }
          }
        }
      }
    }

    stage('Test') {
      matrix {
        agent any

        axes {
          axis {
            name 'TEST_ID'
            values '10', '11', '22', '23', '24', '29', '30', '31', '32', '33', '34', '35', '36', '37', '38', '39', '40', '41', '42', '43', '44', '45', '46', '47', '48', '49', '50', '51', '52', '53', '54', '55', '56', '57', '59', '60', '76', '77', '78', '90', '91', '92', '101', '102', '103', '104', '105', '106', '107', '108', '109', '110', '111', '112', '113', '114', '115', '116', '117', '118', '119', '120', '121', '122', '123', '124', '125'
          }
          axis {
            name 'STAR_BASE'
            values 'root5', 'root6'
          }
          axis {
            name 'COMPILER'
            values 'gcc485'
          }
        }

        stages {
          stage('Run executest.py') {
            steps {
              unstash "star-sw-${STAR_BASE}-${COMPILER}"
              sh '''#!/usr/bin/env bash
                set -euxo pipefail
                starenv="${STAR_BASE}-${COMPILER}"
                image_tag="${IMAGE_PREFIX}-${starenv}"
                image_tar="${ARTIFACT_DIR}/star-sw-${starenv}.tar"
                data_container="star-test-data-${BUILD_TAG}-${starenv}-${TEST_ID}"
                data_container="$(printf '%s' "${data_container}" | tr -c '[:alnum:]_.-' '-')"

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
      }
    }

    stage('ROOT5 test doEvents') {
      matrix {
        agent any

        axes {
          axis {
            name 'TEST_ID'
            values '121', '122'
          }
          axis {
            name 'COMPILER'
            values 'gcc485', 'gcc11'
          }
        }

        stages {
          stage('Run doEvents.C') {
            steps {
              unstash "star-sw-root5-${COMPILER}"
              sh '''#!/usr/bin/env bash
                set -euxo pipefail
                starenv="root5-${COMPILER}"
                image_tag="${IMAGE_PREFIX}-${starenv}"
                image_tar="${ARTIFACT_DIR}/star-sw-${starenv}.tar"
                data_container="star-test-data-${BUILD_TAG}-doEvents-${starenv}-${TEST_ID}"
                data_container="$(printf '%s' "${data_container}" | tr -c '[:alnum:]_.-' '-')"

                docker load --input "${image_tar}"
                docker rm -f "${data_container}" >/dev/null 2>&1 || true
                trap 'docker rm -f "${data_container}" >/dev/null 2>&1 || true' EXIT
                docker run --name "${data_container}" --volume /star "${TEST_DATA_IMAGE}"

                TEST_FILE="$(docker run --rm "${image_tag}" tests/executest.py "${TEST_ID}" -a fullpath | sed -E 's/\\.(daq|fzd)$/.event.root/')"
                TEST_CMD="root4star -b -q -l 'StRoot/macros/analysis/doEvents.C(100, \\"${TEST_FILE}\\")'"
                docker run --volumes-from "${data_container}" "${image_tag}" \
                  sh -c "set -e; ${TEST_CMD} 2>&1 | tee log; grep '<StIOMaker::Finish> IO:' log"
              '''
            }
          }
        }
      }
    }

    stage('ROOT5 test find vertex') {
      matrix {
        agent any

        axes {
          axis {
            name 'TEST_ID'
            values '102', '121', '122'
          }
          axis {
            name 'COMPILER'
            values 'gcc485', 'gcc11'
          }
        }

        stages {
          stage('Run find_vertex.C') {
            steps {
              unstash "star-sw-root5-${COMPILER}"
              sh '''#!/usr/bin/env bash
                set -euxo pipefail
                starenv="root5-${COMPILER}"
                image_tag="${IMAGE_PREFIX}-${starenv}"
                image_tar="${ARTIFACT_DIR}/star-sw-${starenv}.tar"
                data_container="star-test-data-${BUILD_TAG}-find-vertex-${starenv}-${TEST_ID}"
                data_container="$(printf '%s' "${data_container}" | tr -c '[:alnum:]_.-' '-')"

                docker load --input "${image_tar}"
                docker rm -f "${data_container}" >/dev/null 2>&1 || true
                trap 'docker rm -f "${data_container}" >/dev/null 2>&1 || true' EXIT
                docker run --name "${data_container}" --volume /star "${TEST_DATA_IMAGE}"

                TEST_FILE="$(docker run --rm "${image_tag}" tests/executest.py "${TEST_ID}" -a fullpath | sed -E 's/\\.(daq|fzd)$/.event.root/')"
                TEST_CMD="root4star -b -q -l 'StRoot/macros/analysis/find_vertex.C(\\"${TEST_FILE}\\")'"
                docker run --volumes-from "${data_container}" "${image_tag}" \
                  sh -c "set -e; ${TEST_CMD} 2>&1 | tee log; grep '<StIOMaker::Finish> StIO:' log"
              '''
            }
          }
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
