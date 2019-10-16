# MeowRust

> Relaxed Rust variant that even cats can master

---

## What is this

This is a educational compiler for the MeowRust language, a minimal Rust-like language. MeowRust is different from rust in the following ways:
- MeowRust doesn't enforce borrow checking, but will grammatically accepts lifetime notations.
- MeowRust doesn't have capturing closures.
- MeowRust doesn't have macros, and doesn't support most attributes.
- MeowRust doesn't have first-class multi-threading support, so no Send & Sync & friends.
- MeowRust introduces an new operator to do std::transmute
- MeowRust requires explict acknowledgement for drops and invoking non-default deref implementation

TL;DR: Very unsafe and verbose rust.

## Motivation

MeowRust is created as a lab project for Tsinghua Univ compiler course.

Our primary intention is to make Rust more suitable for writing bare-metal softwares, because we would like to get Rust's expressiveness when writing softwares like the second stage bootloader for our archtecture course, or router's control plane for our networking course.

## Distribution

All source code under this repository is distributed under the MIT license. A full copy of the license can be found in the LICENSE file
