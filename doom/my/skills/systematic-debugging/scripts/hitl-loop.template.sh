#!/usr/bin/env bash
set -euo pipefail

# Human-in-the-Loop (HITL) Debugging Reproduction Script Template
#
# Purpose: Facilitate structured, reproducible Human-in-the-Loop debugging sessions
# with interactive step execution and automatic evidence/state capture.

LOG_DIR="${LOG_DIR:-./debug_captures_$(date +%Y%m%d_%H%M%S)}"
mkdir -p "$LOG_DIR"

echo "======================================================"
echo "      HITL Debugging Reproduction Session Started"
echo "======================================================"
echo "Captures directory: $LOG_DIR"
echo "------------------------------------------------------"

# Step Helper Function
# Usage: step "Step description" [optional_command_to_execute]
step() {
  local description="$1"
  local cmd="${2:-}"

  echo ""
  echo "========== [STEP] $description =========="
  if [ -n "$cmd" ]; then
    echo "Executing: $cmd"
    eval "$cmd"
  else
    read -rp "Press [ENTER] after completing this manual step..." _
  fi
}

# Capture Helper Function
# Usage: capture "capture_name" [target_command_or_file]
capture() {
  local name="$1"
  local target="${2:-}"
  local output_file="$LOG_DIR/${name}_$(date +%H%M%S).log"

  echo "[CAPTURE] Capturing context for '$name' -> $output_file"
  {
    echo "=== CAPTURE: $name ==="
    echo "Timestamp: $(date -Iseconds 2>/dev/null || date)"
    echo "PWD: $(pwd)"
    echo "------------------------------------------------------"
    if [ -n "$target" ]; then
      if [ -f "$target" ]; then
        echo "File Content ($target):"
        cat "$target"
      else
        echo "Command Output ($target):"
        eval "$target" 2>&1 || true
      fi
    else
      echo "Environment Summary:"
      env | sort
    fi
  } > "$output_file"
  echo "[CAPTURE] Done -> $output_file"
}

# ======================================================
# REPRODUCTION WORKFLOW TEMPLATE
# ======================================================

# 1. Baseline Initialization & Capture
step "Initialize baseline environment"
capture "01_baseline_env"

# 2. Trigger Action
step "Trigger reproduction action" "echo 'Simulating bug reproduction step...'"
capture "02_post_trigger_state" "git status --short"

step "Perform manual inspection or human-driven interaction"
capture "03_manual_verification"

echo ""
echo "======================================================"
echo "HITL Session Complete. Output captured in $LOG_DIR"
echo "======================================================"
