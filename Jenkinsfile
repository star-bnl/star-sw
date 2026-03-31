pipeline {
  agent none

  options {
    timestamps()
    skipDefaultCheckout(true)
    parallelsAlwaysFailFast()
  }

  parameters {
    string(name: 'BRANCH_NAME', defaultValue: 'main', description: 'Branch to build')
    string(name: 'GIT_COMMIT', defaultValue: '', description: 'Commit SHA to build, optional')
    string(name: 'REPO_URL', defaultValue: 'https://github.com/star-bnl/star-sw.git', description: 'Repository URL')
  }

  environment {
    ARTIFACT_DIR = 'artifacts'
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
              checkout([
                $class: 'GitSCM',
                branches: [[name: "*/${params.BRANCH_NAME}"]],
                userRemoteConfigs: [[
                  url: "${params.REPO_URL}",
                  refspec: '+refs/heads/*:refs/remotes/origin/* +refs/tags/*:refs/tags/*'
                ]]
              ])
            }
          }

          stage('Checkout exact commit if provided') {
            when {
              expression { return params.GIT_COMMIT?.trim() }
            }
            steps {
              sh '''
                set -euxo pipefail
                git fetch --all --tags
                git checkout "${GIT_COMMIT}"
                git rev-parse HEAD
              '''
            }
          }

          stage('Prepare Docker Buildx') {
            steps {
              sh '''
                set -euxo pipefail
                command -v docker
                docker --version
                docker buildx version
                docker buildx use default || true
                docker buildx inspect default --bootstrap || docker buildx inspect --bootstrap
              '''
            }
          }

          stage('Build Docker Image') {
            steps {
              sh '''
                set -euxo pipefail
                starenv="${STAR_BASE}-${COMPILER}"
                image_tag="ghcr.io/star-bnl/star-sw-${starenv}"
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

          stage('Archive Docker Image') {
            steps {
              archiveArtifacts artifacts: "artifacts/star-sw-${STAR_BASE}-${COMPILER}.tar", fingerprint: true
            }
          }
        }
      }
    }
  }

  post {
    always {
      echo "Build finished: ${currentBuild.currentResult}"
    }
  }
}
