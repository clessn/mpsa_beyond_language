#!/usr/bin/env bash
# Completes the translator check (src/43) to three runs for the models served
# through OpenRouter, one process per model, and watches them every minute. A
# process that stops before writing "SCORING COMPLETE", or saves nothing for
# 15 minutes, is restarted (at most 5 times per model) and resumes from its
# own score file. When every model is done, the scores are merged and src/43,
# src/44 and src/87 are run again.
#
# Usage (from the project root):  bash src/43b_run_parallel.sh
# Status: data/tmp/llm_translation/parallel/status.log

set -u
models="gptoss20b qwen332b llama4scout gptoss120b qwen3235b deepseekv32 mistralsmall32 mistrallarge4"
dir="data/tmp/llm_translation/parallel"
mkdir -p "$dir"
status="$dir/status.log"
export N_RUNS_OPENROUTER=3
# Pinned endpoints that no longer serve the model, or are rate-limited, on
# 2026-11-05, replaced at the same precision except Llama 4 Scout, which is
# no longer served in bf16 by any provider (fp8 instead)
export PIN_OVERRIDES="gptoss120b=deepinfra/bf16,gptoss20b=dekallm/bf16,qwen3235b=parasail/fp8,llama4scout=deepinfra/fp8"
stall_seconds=900
max_restarts=5

log() { echo "$(date '+%Y-%m-%d %H:%M:%S') $*" >> "$status"; }

launch() {
  echo "=== launch $(date '+%H:%M:%S')" >> "$dir/$1.log"
  ONLY_MODELS=$1 nohup Rscript src/43_llm_translation_test.R >> "$dir/$1.log" 2>&1 &
  echo $! > "$dir/$1.pid"
  date +%s > "$dir/$1.started"
  log "started $1 (pid $!)"
}

alive() { kill -0 "$(cat "$dir/$1.pid")" 2>/dev/null; }

# "SCORING COMPLETE" written since the last launch
finished() { awk '/^=== launch/{done=0} /SCORING COMPLETE/{done=1} END{exit !done}' "$dir/$1.log"; }

last_save() {
  local f="data/tmp/llm_translation/scores_en_llm_$1.rds"
  if [ -f "$f" ] && [ "$(stat -f %m "$f")" -gt "$(cat "$dir/$1.started")" ]; then
    stat -f %m "$f"
  else
    cat "$dir/$1.started"
  fi
}

progress() {
  MODELS="$models" Rscript -e '
    translated <- !is.na(readRDS("data/tmp/llm_translation/sentences_en_llm.rds"))
    main <- readRDS("data/tmp/llm_translation/scores_en_llm.rds")
    out <- sapply(strsplit(Sys.getenv("MODELS"), " ")[[1]], function(m) {
      f <- sprintf("data/tmp/llm_translation/scores_en_llm_%s.rds", m)
      s <- if (file.exists(f)) readRDS(f) else main
      cols <- intersect(sprintf("%s_en_llm_run%d", m, 1:3), names(s))
      sprintf("%s %d/%d", m, sum(sapply(cols, function(c) sum(!is.na(s[[c]]) & translated))), 3 * sum(translated))
    })
    cat(paste(out, collapse = "  "))' 2>/dev/null
}

log "=== parallel run: $models"
for m in $models; do
  echo 0 > "$dir/$m.restarts"
  rm -f "$dir/$m.gaveup"
  launch "$m"
  sleep 2
done

while true; do
  sleep 60
  pending=0
  for m in $models; do
    { finished "$m" || [ -f "$dir/$m.gaveup" ]; } && continue
    pending=$((pending + 1))
    idle=$(( $(date +%s) - $(last_save "$m") ))
    alive "$m" && [ "$idle" -lt "$stall_seconds" ] && continue
    restarts=$(cat "$dir/$m.restarts")
    if [ "$restarts" -ge "$max_restarts" ]; then
      alive "$m" && kill "$(cat "$dir/$m.pid")" 2>/dev/null
      touch "$dir/$m.gaveup"
      log "giving up on $m after $restarts restarts"
      continue
    fi
    if alive "$m"; then
      log "$m saved nothing for ${idle}s, restarting it"
      kill "$(cat "$dir/$m.pid")" 2>/dev/null
      sleep 5
    else
      log "$m stopped before finishing, restarting it (tail: $(tail -n 1 "$dir/$m.log" | cut -c1-100))"
    fi
    echo $((restarts + 1)) > "$dir/$m.restarts"
    launch "$m"
  done
  log "progress: $(progress)"
  [ "$pending" -eq 0 ] && break
done

gave_up=$(ls "$dir"/*.gaveup 2>/dev/null | wc -l | tr -d ' ')
log "scoring finished ($gave_up model(s) given up); merging and rebuilding Table 1 and the figures"
if Rscript src/43_llm_translation_test.R >> "$dir/merge.log" 2>&1 &&
   Rscript src/44_lsd_on_llm_translation.R >> "$dir/merge.log" 2>&1 &&
   Rscript src/87_tables_figures.R >> "$dir/merge.log" 2>&1; then
  log "ALL DONE"
else
  log "merge failed, see $dir/merge.log"
fi
