---
title: Deploying Haskell Services
---

# Deploying Haskell Services

* Michael Snoyman
* VP Engineering, FP Complete<br><img alt="FP Complete logo" src="https://tech.fpcomplete.com/images/fp-complete-logo-small.png" style="border:0">
* Amsterdam Haskell meetup
* Thursday, September 19, 2019

---

## Haskell network services

* Use lots of Haskell
* Mostly network services
* Build/deploy/monitor
* First question... why Haskell?

---

## Why Haskell (in general)?

* Reliable code
* Highly productive
* Easy to scale to a large team
* Avoid large classes of bugs

---

## Why Haskell (network services)?

* Green threads
* Efficient runtime/servers like Warp
* Immutability, purity, and STM
* Low memory footprint
* Low CPU overhead
* Compiled executable

---

## Simplified Haskell deployment

* Compile executable locally
* Spin up VPS
* `scp` executable to VPS
* `ssh` in, run `screen`, run executable
* Done!

---

## Problems

* System libraries, `libc` and `gmp` version mismatch
* Associated assets
* Unreliable testing story
* Downtime during deployment
* Non-resilient to machine failure
* Non-resilient to cloud provider failure

---

## Solving these problems

* Lots of approaches
* Not a Haskell-specific problem
* At FP Complete: we use general purpose solutions
* Docker, Kubernetes, AWS
* Let's step through standard deployment
* Conserve the novelty budget!

---

## CI

* We like Gitlab CI
* Keep the configuration in the repo
* Build with a Docker image too
* `stack.yaml` guarantees identical build plan
* Just last week: rebuilt a web app with a minor tweak still using GHC 7.8
* Artifact: a Docker image for runtime

---

## Testing

* Types are great, but you still need to test!
* Especially integration/system tests
* Test the exact Docker image you're about to deploy to production

---

## Staging

* Separate CI and production Kubernetes cluster
* Gitlab review apps
* Able to deploy branches to staging cluster and see difference
* Great for interacting with product owners and customers

---

## Production

* Use Kubernetes `deployment` system
* Tests each pod
* Zero downtime for deployment
* Concern: share mutable resources like databases
    * Moral of the story: mutability always sucks

---

## High availability

* Multi-node
* Multi-AZ
* Load balancer
* Auto-scaling group

---

## Configuration

* YAML config files (`Data.Yaml.Config`)
    * Allows environment variable overrides
* Secret credentials (e.g. database passwords) use Kube secrets
    * Currently moving over to Vault
* Can run different configurations in staging and prod

---

## Logging

* Apps dump to `stdout`/`stderr`
* We use `rio` and its logging capabilities
* Sometimes use structured logging (with or without `rio`)
* Control log level with environment variables
* Use standard log aggregation tools

---

## Monitoring

* Prometheus
* Monitor standard node and cluster level stats by default
    * Memory, CPU, etc
* Monitor ingress and egress (network traffic, status codes)
* Some apps: add custom monitoring info as desired

---

## Summary

* Very little is Haskell specific
* Use common DevOps tools, providers, services
* Easy to integrate with existing infrastructure
* Haskell + DevOps = reliable services

---

## Questions

* Thanks everyone!
