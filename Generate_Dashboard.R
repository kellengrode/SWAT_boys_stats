
# ------------------------------------------------------------
# 0. Run data ingest if data is stale (older than 7 days)
# ------------------------------------------------------------

ingest_script <- "data_ingest.R"   # adjust path if needed
cache_file    <- "combined_df.rds" # saved after ingest to track freshness

needs_refresh <- TRUE  # default to refresh

# uncomment this part and run to first to force a refresh w/ less than 7 days
# file.remove("combined_df.rds")
# source("dashboard.R")

if (file.exists(cache_file)) {
  last_modified <- file.info(cache_file)$mtime
  age_days      <- as.numeric(difftime(Sys.time(), last_modified, units = "days"))
  
  if (age_days < 7) {
    message("Data is ", round(age_days, 1), " days old — loading from cache, skipping ingest.")
    combined_df  <- readRDS(cache_file)
    needs_refresh <- FALSE
  } else {
    message("Data is ", round(age_days, 1), " days old — refreshing from Google Sheets.")
  }
}

if (needs_refresh) {
  source(ingest_script)
  saveRDS(combined_df, cache_file)
  message("Ingest complete. Cache saved to: ", normalizePath(cache_file))
}

# ============================================================
# SWAT Boys — Self-Contained Interactive HTML Dashboard
# v3: All-Time tab, removed SB, Miami Dolphins color scheme
# ============================================================
# Requirements: jsonlite, dplyr, stringr
# install.packages(c("jsonlite", "dplyr", "stringr"))

library(jsonlite)
library(dplyr)
library(stringr)

# ------------------------------------------------------------
# 1. Prepare per-season data
# ------------------------------------------------------------

dashboard_data <- combined_df |>
  mutate(
    BBK    = ifelse(!is.na(BB) & !is.na(SO) & SO > 0, round(BB / SO, 3), NA_real_),
    QABpct = ifelse(!is.na(QAB) & !is.na(PA) & PA > 0, round(QAB / PA, 3), NA_real_),
    across(c(PA, AB, H, HR, RBI, R, BB, SO, QAB,
             AVG, OBP, SLG, OPS), as.numeric),
    season_label = source_tab
  ) |>
  select(
    season_label, season, year, First, Last,
    PA, AB, H, HR, RBI, R,
    AVG, OBP, SLG, OPS,
    BB, SO, QAB,
    BBK, QABpct,
    any_of(c("1B","2B","3B","HHB","LD_pct","GB_pct","FB_pct","BABIP","XBH","TB","HBP","SAC","SF"))
  ) |>
  filter(!is.na(PA))

# ------------------------------------------------------------
# 2. Build all-time data
#    Counting stats: summed across all seasons
#    Rate stats: PA-weighted average across all seasons
# ------------------------------------------------------------

alltime_data <- dashboard_data |>
  group_by(First, Last) |>
  summarise(
    season_label   = "All Time",
    seasons_played = n_distinct(season_label),
    PA   = sum(PA,  na.rm = TRUE),
    AB   = sum(AB,  na.rm = TRUE),
    H    = sum(H,   na.rm = TRUE),
    X1B  = sum(.data[["1B"]], na.rm = TRUE),
    X2B  = sum(.data[["2B"]], na.rm = TRUE),
    X3B  = sum(.data[["3B"]], na.rm = TRUE),
    HR   = sum(HR,  na.rm = TRUE),
    RBI  = sum(RBI, na.rm = TRUE),
    R    = sum(R,   na.rm = TRUE),
    BB   = sum(BB,  na.rm = TRUE),
    SO   = sum(SO,  na.rm = TRUE),
    QAB  = sum(QAB, na.rm = TRUE),
    # Rate stats calculated from aggregated raw counts, not weighted averages
    # AVG = total H / total AB
    AVG    = ifelse(sum(AB, na.rm=TRUE) > 0,
                    round(sum(H, na.rm=TRUE) / sum(AB, na.rm=TRUE), 3), NA_real_),
    # OBP = (H + BB) / PA  (full formula adds HBP and subtracts SF, adjust if available)
    OBP    = ifelse(sum(PA, na.rm=TRUE) > 0,
                    round((sum(H, na.rm=TRUE) + sum(BB, na.rm=TRUE)) / sum(PA, na.rm=TRUE), 3), NA_real_),
    # SLG = total TB / total AB — uses TB column if present, else 2B/3B breakdown, else H+3*HR fallback
    SLG    = {
      tot_ab <- sum(AB, na.rm=TRUE)
      tb <- if ("TB" %in% names(pick(everything())))
        sum(TB, na.rm=TRUE)
      else if (all(c("X2B","X3B") %in% names(pick(everything()))))
        sum(H,na.rm=TRUE) + sum(X2B,na.rm=TRUE) + 2*sum(X3B,na.rm=TRUE) + 3*sum(HR,na.rm=TRUE)
      else
        sum(H, na.rm=TRUE) + 3*sum(HR, na.rm=TRUE)
      if (tot_ab > 0) round(tb / tot_ab, 3) else NA_real_
    },
    OPS    = ifelse(!is.na(OBP) & !is.na(SLG), round(OBP + SLG, 3), NA_real_),
    # BB/K = total BB / total SO
    BBK    = ifelse(sum(SO, na.rm=TRUE) > 0,
                    round(sum(BB, na.rm=TRUE) / sum(SO, na.rm=TRUE), 3), NA_real_),
    # QAB% = total QAB / total PA
    QABpct = ifelse(sum(PA, na.rm=TRUE) > 0,
                    round(sum(QAB, na.rm=TRUE) / sum(PA, na.rm=TRUE), 3), NA_real_),
    .groups = "drop"
  ) |>
  filter(PA > 0)

# ------------------------------------------------------------
# 3. Season list — Spring before Fall within each year
# ------------------------------------------------------------

seasons_ordered <- dashboard_data |>
  distinct(season_label, year, season) |>
  mutate(season_order = ifelse(season == "Spring", 1L, 2L)) |>
  arrange(year, season_order) |>
  pull(season_label)

# ------------------------------------------------------------
# 4. Serialise to JSON
# ------------------------------------------------------------

data_json     <- toJSON(dashboard_data, na = "null", auto_unbox = TRUE)
seasons_json  <- toJSON(seasons_ordered, auto_unbox = FALSE)
alltime_json  <- toJSON(alltime_data,  na = "null", auto_unbox = TRUE)

# ------------------------------------------------------------
# 5. Build HTML — three raw string chunks, JSON injected between
#    Uses r"[...]" delimiters — safe against )" in HTML/JS
# ------------------------------------------------------------

chunk1 <- r"[<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="UTF-8">
<meta name="viewport" content="width=device-width, initial-scale=1.0">
<title>SWAT Boys &mdash; Stats Dashboard</title>
<script src="https://cdnjs.cloudflare.com/ajax/libs/Chart.js/4.4.1/chart.umd.js"></script>
<style>
  @import url("https://fonts.googleapis.com/css2?family=DM+Mono:wght@400;500&family=Barlow+Condensed:wght@400;600;700&family=Barlow:wght@400;500&display=swap");
  :root {
    /* Miami Dolphins palette */
    --bg:      #0a0e13;
    --bg2:     #111820;
    --bg3:     #18232e;
    --bg4:     #1e2d3a;
    --steel:   #2e4457;
    --orange:  #F26522;
    --orange2: #FF8C42;
    --blue:    #008DB9;
    --blue2:   #00B4E0;
    --aqua:    #00E5CC;
    --text:    #e8edf2;
    --muted:   #7a9ab5;
    --dim:     #3a5570;
    --border:  rgba(0,141,185,0.18);
  }
  *,*::before,*::after{box-sizing:border-box;margin:0;padding:0;}
  body{font-family:"Barlow",sans-serif;background:var(--bg);color:var(--text);min-height:100vh;}

  /* HEADER */
  .site-header{background:var(--bg2);border-bottom:1px solid var(--border);padding:0 2rem;
    display:flex;align-items:center;justify-content:space-between;height:56px;
    position:sticky;top:0;z-index:100;}
  .logo{font-family:"Barlow Condensed",sans-serif;font-weight:700;font-size:20px;
    letter-spacing:0.06em;color:var(--orange);text-transform:uppercase;}
  .logo span{color:var(--blue2);font-weight:400;}
  .header-badge{font-family:"DM Mono",monospace;font-size:11px;color:var(--muted);
    background:var(--bg3);border:1px solid var(--border);padding:4px 10px;border-radius:4px;}

  /* TABS */
  .tab-bar{display:flex;background:var(--bg2);border-bottom:1px solid var(--border);
    padding:0 2rem;overflow-x:auto;}
  .tab-btn{font-family:"Barlow Condensed",sans-serif;font-size:13px;font-weight:600;
    letter-spacing:0.08em;text-transform:uppercase;color:var(--muted);background:none;
    border:none;border-bottom:2px solid transparent;padding:12px 20px;cursor:pointer;
    white-space:nowrap;transition:color 0.15s,border-color 0.15s;}
  .tab-btn:hover{color:var(--blue2);}
  .tab-btn.active{color:var(--orange);border-bottom-color:var(--orange);}

  /* PANES */
  .pane{display:none;padding:1.5rem 2rem 3rem;}
  .pane.active{display:block;}

  /* CONTROLS */
  .controls{display:flex;flex-wrap:wrap;gap:12px;margin-bottom:1.5rem;align-items:flex-end;}
  .ctrl-group{display:flex;flex-direction:column;gap:5px;}
  .ctrl-label{font-family:"DM Mono",monospace;font-size:10px;letter-spacing:0.1em;
    text-transform:uppercase;color:var(--muted);}
  select{font-family:"Barlow",sans-serif;font-size:13px;padding:7px 12px;border-radius:4px;
    border:1px solid var(--steel);background:var(--bg3);color:var(--text);
    cursor:pointer;min-width:160px;}
  select:focus{outline:none;border-color:var(--blue);}

  /* METRIC TILES */
  .metric-grid{display:grid;grid-template-columns:repeat(6,minmax(0,1fr));gap:10px;margin-bottom:1.5rem;}
  .metric-card{background:var(--bg3);border:1px solid var(--border);border-radius:6px;padding:12px 14px;}
  .metric-card.accent-top{border-top:2px solid var(--orange);}
  .metric-card.blue-top{border-top:2px solid var(--blue);}
  .m-label{font-family:"DM Mono",monospace;font-size:10px;color:var(--muted);margin-bottom:4px;letter-spacing:0.06em;}
  .m-value{font-family:"Barlow Condensed",sans-serif;font-size:24px;font-weight:700;color:var(--orange);line-height:1;}
  .m-sub{font-size:11px;color:var(--dim);margin-top:3px;}

  /* PANELS */
  .panel{background:var(--bg2);border:1px solid var(--border);border-radius:8px;padding:1.25rem 1.5rem;}
  .panel-title{font-family:"Barlow Condensed",sans-serif;font-size:12px;font-weight:600;
    letter-spacing:0.1em;text-transform:uppercase;color:var(--muted);margin-bottom:1rem;}
  .two-col{display:grid;grid-template-columns:1fr 1.8fr;gap:16px;margin-bottom:1.5rem;}
  .chart-wrap{position:relative;width:100%;}

  /* STAT TABLE */
  .stat-table-wrap{overflow-x:auto;}
  .stat-table{width:100%;border-collapse:collapse;font-size:12px;}
  .stat-table th{font-family:"DM Mono",monospace;font-size:10px;letter-spacing:0.07em;
    color:var(--muted);text-align:right;padding:6px 10px;border-bottom:1px solid var(--border);white-space:nowrap;}
  .stat-table th:first-child{text-align:left;}
  .stat-table td{text-align:right;padding:6px 10px;color:var(--muted);
    border-bottom:1px solid rgba(0,141,185,0.07);white-space:nowrap;
    font-family:"DM Mono",monospace;font-size:11px;}
  .stat-table td:first-child{text-align:left;color:var(--text);font-family:"Barlow",sans-serif;
    font-size:12px;font-weight:500;}
  .stat-table tr:hover td{background:var(--bg3);}
  .stat-table .top-val{color:var(--orange);font-weight:500;}

  /* STAT PILLS */
  .stat-pills{display:flex;flex-wrap:wrap;gap:7px;}
  .stat-pill{font-family:"DM Mono",monospace;font-size:11px;padding:5px 12px;
    border-radius:20px;border:1px solid var(--steel);background:transparent;
    color:var(--muted);cursor:pointer;transition:all 0.15s;}
  .stat-pill:hover{border-color:var(--blue2);color:var(--blue2);}
  .stat-pill.active-0{background:var(--orange);border-color:var(--orange);color:#fff;}
  .stat-pill.active-1{background:var(--blue);border-color:var(--blue);color:#fff;}
  .stat-pill.active-2{background:var(--aqua);border-color:var(--aqua);color:#0a0e13;}
  .stat-pill.disabled{opacity:0.3;cursor:not-allowed;}

  /* TIMELINE CARDS */
  .tl-cards{display:grid;grid-template-columns:repeat(3,minmax(0,1fr));gap:10px;margin-top:1rem;}
  .tl-card{background:var(--bg3);border-left:3px solid var(--dim);border-radius:0 6px 6px 0;padding:10px 14px;}
  .tl-card.c0{border-left-color:var(--orange);}
  .tl-card.c1{border-left-color:var(--blue);}
  .tl-card.c2{border-left-color:var(--aqua);}

  /* RBI / LEADER BARS */
  .rbi-row{display:flex;align-items:center;gap:10px;margin-bottom:7px;}
  .rbi-name{font-size:12px;color:var(--muted);width:80px;flex-shrink:0;text-align:right;
    overflow:visible;text-overflow:ellipsis;white-space:nowrap;padding-right:7px; }
  .rbi-track{flex:1;background:var(--bg4);border-radius:3px;height:18px;overflow:hidden;}
  .rbi-fill{height:100%;background:var(--orange);border-radius:3px;display:flex;
    align-items:center;justify-content:flex-end;padding-right:7px;}
  .rbi-val{font-family:"DM Mono",monospace;font-size:11px;color:#fff;font-weight:500;}

  /* ALL-TIME SECTION */
  .at-section-title{font-family:"Barlow Condensed",sans-serif;font-size:13px;font-weight:600;
    letter-spacing:0.1em;text-transform:uppercase;color:var(--blue2);
    margin:1.5rem 0 0.75rem;border-bottom:1px solid var(--border);padding-bottom:6px;}

  /* LEGEND */
  .legend{display:flex;flex-wrap:wrap;gap:14px;margin-top:12px;}
  .legend-item{display:flex;align-items:center;gap:6px;font-size:12px;color:var(--muted);}
  .legend-line{width:18px;height:3px;border-radius:2px;}

  @media(max-width:700px){
    .metric-grid{grid-template-columns:repeat(3,1fr);}
    .two-col{grid-template-columns:1fr;}
    .tl-cards{grid-template-columns:repeat(2,1fr);}
    .pane{padding:1rem 1rem 2rem;}
    .tab-bar,.site-header{padding:0 1rem;}
  }
</style>
</head>
<body>

<header class="site-header">
  <div class="logo">SWAT Boys <span>Stats</span></div>
  <div class="header-badge">Men&apos;s Softball &nbsp;&middot;&nbsp; 2022&ndash;2025</div>
</header>

<nav class="tab-bar">
  <button class="tab-btn active" data-tab="summary"  onclick="showTab(this)">Season Summary</button>
  <button class="tab-btn"        data-tab="alltime"  onclick="showTab(this)">All Time</button>
  <button class="tab-btn"        data-tab="timeline" onclick="showTab(this)">Player Timeline</button>
  <button class="tab-btn"        data-tab="ranker"   onclick="showTab(this)">Stat Ranker</button>
</nav>

<!-- TAB 1: SEASON SUMMARY -->
<div id="pane-summary" class="pane active">
  <div class="controls">
    <div class="ctrl-group">
      <div class="ctrl-label">Season</div>
      <select id="sum-season" onchange="renderSummary()"></select>
    </div>
  </div>
  <div class="metric-grid" id="sum-metrics"></div>
  <div class="two-col">
    <div class="panel">
      <div class="panel-title">RBI Leaders</div>
      <div id="sum-rbi"></div>
    </div>
    <div class="panel">
      <div class="panel-title">Roster &mdash; sorted by PA</div>
      <div class="stat-table-wrap">
        <table class="stat-table" id="sum-table"></table>
      </div>
    </div>
  </div>
</div>

<!-- TAB 2: ALL TIME -->
<div id="pane-alltime" class="pane">
  <div class="at-section-title">Career Totals &mdash; Counting Stats</div>
  <div class="metric-grid" id="at-metrics" style="grid-template-columns:repeat(5,minmax(0,1fr));"></div>

  <div class="two-col">
    <div class="panel">
      <div class="panel-title">All-Time RBI Leaders</div>
      <div id="at-rbi"></div>
    </div>
    <div class="panel">
      <div class="panel-title">All-Time HR Leaders</div>
      <div id="at-hr"></div>
    </div>
  </div>

  <div class="at-section-title">Career Rate Stats &mdash; PA-Weighted Averages</div>
  <div class="panel">
    <div class="panel-title">Career roster &mdash; sorted by PA</div>
    <div class="stat-table-wrap">
      <table class="stat-table" id="at-table"></table>
    </div>
  </div>
</div>

<!-- TAB 3: PLAYER TIMELINE -->
<div id="pane-timeline" class="pane">
  <div class="controls">
    <div class="ctrl-group">
      <div class="ctrl-label">Player</div>
      <select id="tl-player" onchange="renderTimeline()"></select>
    </div>
    <div class="ctrl-group">
      <div class="ctrl-label">Stats &mdash; up to 3</div>
      <div class="stat-pills" id="tl-pills"></div>
    </div>
  </div>
  <div class="panel" style="margin-bottom:1rem;">
    <div class="chart-wrap" style="height:320px;"><canvas id="tl-chart"></canvas></div>
    <div class="legend" id="tl-legend"></div>
  </div>
  <div class="tl-cards" id="tl-cards"></div>
</div>

<!-- TAB 4: STAT RANKER -->
<div id="pane-ranker" class="pane">
  <div class="controls">
    <div class="ctrl-group">
      <div class="ctrl-label">Stat</div>
      <select id="rk-stat" onchange="renderRanker()">
        <option value="AVG">AVG &mdash; Batting average</option>
        <option value="OPS">OPS &mdash; On-base + slugging</option>
        <option value="OBP">OBP &mdash; On-base percentage</option>
        <option value="SLG">SLG &mdash; Slugging percentage</option>
        <option value="RBI">RBI &mdash; Runs batted in</option>
        <option value="HR">HR &mdash; Home runs</option>
        <option value="QABpct">QAB% &mdash; Quality at bat rate</option>
        <option value="BBK">BB/K &mdash; Walks per strikeout</option>
      </select>
    </div>
    <div class="ctrl-group">
      <div class="ctrl-label">Season</div>
      <select id="rk-season" onchange="renderRanker()"></select>
    </div>
    <div class="ctrl-group">
      <div class="ctrl-label">Min PA</div>
      <select id="rk-minpa" onchange="renderRanker()">
        <option value="0">All players</option>
        <option value="10" selected>10+ PA</option>
        <option value="20">20+ PA</option>
        <option value="30">30+ PA</option>
      </select>
    </div>
  </div>
  <div class="metric-grid" id="rk-metrics" style="grid-template-columns:repeat(4,minmax(0,1fr));"></div>
  <div class="panel">
    <div class="chart-wrap"><canvas id="rk-chart"></canvas></div>
  </div>
</div>

<script>
// ── EMBEDDED DATA ─────────────────────────────────────────
const RAW     = ]"


chunk2 <- r"[;
const SEASONS  = ]"


chunk3 <- r"[;
const ALLTIME  = ]"


chunk4 <- r"[;

// ── STAT METADATA ─────────────────────────────────────────
const STAT_META = {
  AVG:    { label:"AVG",  fmt: v => v != null ? v.toFixed(3)           : "—", isRate:true  },
  OBP:    { label:"OBP",  fmt: v => v != null ? v.toFixed(3)           : "—", isRate:true  },
  SLG:    { label:"SLG",  fmt: v => v != null ? v.toFixed(3)           : "—", isRate:true  },
  OPS:    { label:"OPS",  fmt: v => v != null ? v.toFixed(3)           : "—", isRate:true  },
  RBI:    { label:"RBI",  fmt: v => v != null ? Math.round(v)          : "—", isRate:false },
  HR:     { label:"HR",   fmt: v => v != null ? Math.round(v)          : "—", isRate:false },
  QABpct: { label:"QAB%", fmt: v => v != null ? (v*100).toFixed(1)+"%" : "—", isRate:true  },
  BBK:    { label:"BB/K", fmt: v => v != null ? v.toFixed(2)           : "—", isRate:true  },
};

const COLORS = ["#F26522","#008DB9","#00E5CC"];

// ── TAB SWITCHING ─────────────────────────────────────────
function showTab(btn) {
  const id = btn.dataset.tab;
  document.querySelectorAll(".pane").forEach(p => p.classList.remove("active"));
  document.querySelectorAll(".tab-btn").forEach(b => b.classList.remove("active"));
  document.getElementById("pane-" + id).classList.add("active");
  btn.classList.add("active");
  if (id === "alltime")  renderAllTime();
  if (id === "timeline") renderTimeline();
  if (id === "ranker")   renderRanker();
}

// ── HELPERS ───────────────────────────────────────────────
function bySeason(season) {
  return RAW.filter(r => r.season_label === season && r.PA != null && r.PA > 0);
}
function playerName(r) {
  const l = r.Last || "", f = r.First || "";
  return l ? l + ", " + f.charAt(0) : f;
}
function safeNum(v) {
  return (v == null || isNaN(v)) ? null : +v;
}
function wAvg(rows, key) {
  const valid = rows.filter(r => safeNum(r[key]) != null && safeNum(r.PA) > 0);
  const tot   = valid.reduce((s, r) => s + r.PA, 0);
  return tot > 0 ? valid.reduce((s, r) => s + r[key] * r.PA, 0) / tot : null;
}
function leaderBar(rows, key, maxRows) {
  const sorted = [...rows].filter(r => safeNum(r[key]) != null)
                          .sort((a,b) => b[key] - a[key])
                          .slice(0, maxRows || 8);
  const maxV = sorted.length ? sorted[0][key] : 1;
  return sorted.map(p => {
    const pct = (p[key] / maxV * 100).toFixed(1);
    const nm  = (p.Last || p.First || "").substring(0, 15);
    return "<div class=\"rbi-row\">" +
      "<div class=\"rbi-name\">" + nm + "</div>" +
      "<div class=\"rbi-track\"><div class=\"rbi-fill\" style=\"width:" + pct + "%\">" +
      "<span class=\"rbi-val\">" + Math.round(p[key]) + "</span></div></div></div>";
  }).join("");
}

// ═══════════════════════════════════════════════════════════
// TAB 1 — SEASON SUMMARY
// ═══════════════════════════════════════════════════════════
function renderSummary() {
  const season = document.getElementById("sum-season").value;
  const rows   = bySeason(season).sort((a, b) => b.PA - a.PA);

  const tAVG  = wAvg(rows, "AVG");
  const tOBP  = wAvg(rows, "OBP");
  const tOPS  = wAvg(rows, "OPS");
  const tHR   = rows.reduce((s, r) => s + (safeNum(r.HR)  || 0), 0);
  const tRBI  = rows.reduce((s, r) => s + (safeNum(r.RBI) || 0), 0);
  const tQAB  = wAvg(rows, "QABpct");
  const fR    = (v, d) => v != null ? v.toFixed(d || 3) : "—";

  document.getElementById("sum-metrics").innerHTML = [
    { l:"AVG",  v: fR(tAVG),    s:"team weighted" },
    { l:"OBP",  v: fR(tOBP),    s:"team weighted" },
    { l:"OPS",  v: fR(tOPS),    s:"team weighted" },
    { l:"HR",   v: tHR,          s:"total" },
    { l:"RBI",  v: tRBI,         s:"total" },
    { l:"QAB%", v: tQAB != null ? (tQAB*100).toFixed(1)+"%" : "—", s:"quality AB rate" },
  ].map(m => "<div class=\"metric-card accent-top\">" +
    "<div class=\"m-label\">" + m.l + "</div>" +
    "<div class=\"m-value\">" + m.v + "</div>" +
    "<div class=\"m-sub\">"  + m.s + "</div></div>").join("");

  document.getElementById("sum-rbi").innerHTML = leaderBar(rows, "RBI", 15);

  const cols = ["PA","AB","AVG","OBP","OPS","HR","RBI","BB","SO","QABpct"];
  const hdr  = ["PA","AB","AVG","OBP","OPS","HR","RBI","BB","SO","QAB%"];
  const maxes = {};
  cols.forEach(c => { maxes[c] = Math.max(...rows.map(r => safeNum(r[c]) || 0)); });

  const fmtCell = (r, c) => {
    const v = safeNum(r[c]);
    if (v == null) return "—";
    if (["AVG","OBP","OPS","SLG"].includes(c)) return v.toFixed(3);
    if (c === "QABpct") return (v*100).toFixed(1)+"%";
    return Math.round(v);
  };

  document.getElementById("sum-table").innerHTML =
    "<tr><th>Player</th>" + hdr.map(h => "<th>" + h + "</th>").join("") + "</tr>" +
    rows.map(r =>
      "<tr><td>" + playerName(r) + "</td>" +
      cols.map(c => {
        const v   = safeNum(r[c]);
        const top = v != null && maxes[c] > 0 && v === maxes[c];
        return "<td class=\"" + (top ? "top-val" : "") + "\">" + fmtCell(r, c) + "</td>";
      }).join("") + "</tr>"
    ).join("");
}

// ═══════════════════════════════════════════════════════════
// TAB 2 — ALL TIME
// ═══════════════════════════════════════════════════════════
let atChart = null;

function renderAllTime() {
  const rows = [...ALLTIME].sort((a, b) => b.PA - a.PA);

  const totPA  = rows.reduce((s,r) => s + (safeNum(r.PA)  || 0), 0);
  const totHR  = rows.reduce((s,r) => s + (safeNum(r.HR)  || 0), 0);
  const totRBI = rows.reduce((s,r) => s + (safeNum(r.RBI) || 0), 0);
  const totH   = rows.reduce((s,r) => s + (safeNum(r.H)   || 0), 0);
  const totBB  = rows.reduce((s,r) => s + (safeNum(r.BB)  || 0), 0);
  const totAB  = rows.reduce((s,r) => s + (safeNum(r.AB)  || 0), 0);
  const teamAVG = totAB > 0 ? (totH / totAB).toFixed(3) : "—";

  document.getElementById("at-metrics").innerHTML = [
    { l:"Total PA",  v: totPA,    s:"all seasons" },
    { l:"Team AVG",  v: teamAVG,  s:"career" },
    { l:"Total HR",  v: totHR,    s:"all seasons" },
    { l:"Total RBI", v: totRBI,   s:"all seasons" },
    { l:"Total BB",  v: totBB,    s:"all seasons" },
  ].map(m => "<div class=\"metric-card blue-top\">" +
    "<div class=\"m-label\">" + m.l + "</div>" +
    "<div class=\"m-value\" style=\"color:var(--blue2)\">" + m.v + "</div>" +
    "<div class=\"m-sub\">"  + m.s + "</div></div>").join("");

  document.getElementById("at-rbi").innerHTML = leaderBar(rows, "RBI", 8);
  document.getElementById("at-hr").innerHTML  = leaderBar(rows, "HR",  8);

  const cols = ["PA","AB","H","HR","RBI","BB","SO","AVG","OBP","SLG","OPS","QABpct","BBK"];
  const hdr  = ["PA","AB","H","HR","RBI","BB","SO","AVG","OBP","SLG","OPS","QAB%","BB/K"];
  const maxes = {};
  cols.forEach(c => { maxes[c] = Math.max(...rows.map(r => safeNum(r[c]) || 0)); });

  const fmtCell = (r, c) => {
    const v = safeNum(r[c]);
    if (v == null) return "—";
    if (["AVG","OBP","SLG","OPS"].includes(c)) return v.toFixed(3);
    if (c === "QABpct") return (v*100).toFixed(1)+"%";
    if (c === "BBK") return v.toFixed(2);
    return Math.round(v);
  };

  document.getElementById("at-table").innerHTML =
    "<tr><th>Player</th><th>Seasons</th>" + hdr.map(h => "<th>" + h + "</th>").join("") + "</tr>" +
    rows.map(r =>
      "<tr><td>" + playerName(r) + "</td>" +
      "<td style=\"text-align:right;font-family:DM Mono,monospace;font-size:11px;color:var(--muted)\">" + (r.seasons_played || "—") + "</td>" +
      cols.map(c => {
        const v   = safeNum(r[c]);
        const top = v != null && maxes[c] > 0 && v === maxes[c];
        return "<td class=\"" + (top ? "top-val" : "") + "\">" + fmtCell(r, c) + "</td>";
      }).join("") + "</tr>"
    ).join("");
}

// ═══════════════════════════════════════════════════════════
// TAB 3 — PLAYER TIMELINE
// ═══════════════════════════════════════════════════════════
const TL_STATS = ["AVG","OBP","SLG","OPS","HR","RBI","QABpct"];
let tlSelected = ["AVG","OBP","OPS"];
let tlChart    = null;

function initTimeline() {
  const sel     = document.getElementById("tl-player");
  const players = [...new Set(RAW.map(r => playerName(r)))].sort();
  players.forEach(p => {
    const o = document.createElement("option");
    o.value = p; o.textContent = p; sel.appendChild(o);
  });
  const pills = document.getElementById("tl-pills");
  TL_STATS.forEach(k => {
    const d = document.createElement("div");
    d.className = "stat-pill";
    d.dataset.key = k;
    d.textContent = STAT_META[k].label;
    d.onclick = () => toggleTLStat(k);
    pills.appendChild(d);
  });
  updatePills();
}

function toggleTLStat(key) {
  if (tlSelected.includes(key)) {
    if (tlSelected.length === 1) return;
    tlSelected = tlSelected.filter(s => s !== key);
  } else {
    if (tlSelected.length >= 3) return;
    tlSelected.push(key);
  }
  updatePills();
  renderTimeline();
}

function updatePills() {
  document.querySelectorAll(".stat-pill").forEach(p => {
    const k   = p.dataset.key;
    const idx = tlSelected.indexOf(k);
    p.className = "stat-pill";
    if (idx >= 0)                    p.classList.add("active-" + idx);
    else if (tlSelected.length >= 3) p.classList.add("disabled");
  });
}

function renderTimeline() {
  const selName = document.getElementById("tl-player").value;
  const pRows   = RAW.filter(r => playerName(r) === selName && r.PA > 0);

  const datasets = tlSelected.map((key, si) => {
    const meta = STAT_META[key];
    const vals = SEASONS.map(s => {
      const r = pRows.find(p => p.season_label === s);
      if (!r) return null;
      const v = safeNum(r[key]);
      return v != null ? (meta.isRate ? parseFloat(v.toFixed(4)) : Math.round(v)) : null;
    });
    return {
      label: meta.label, data: vals,
      borderColor: COLORS[si], backgroundColor: COLORS[si] + "22",
      pointBackgroundColor: COLORS[si], pointRadius: 5, pointHoverRadius: 7,
      borderWidth: 2.5, tension: 0.35, fill: false,
      yAxisID: meta.isRate ? "yR" : "yC",
      spanGaps: true,
    };
  });

  const hasR = tlSelected.some(k => STAT_META[k].isRate);
  const hasC = tlSelected.some(k => !STAT_META[k].isRate);

  if (tlChart) tlChart.destroy();
  tlChart = new Chart(document.getElementById("tl-chart"), {
    type: "line",
    data: { labels: SEASONS, datasets },
    options: {
      responsive: true, maintainAspectRatio: false,
      interaction: { mode: "index", intersect: false },
      plugins: {
        legend: { display: false },
        tooltip: {
          backgroundColor: "#0a0e13", titleColor: "#7a9ab5", bodyColor: "#e8edf2",
          borderColor: "rgba(0,141,185,0.3)", borderWidth: 1, padding: 10,
          callbacks: {
            label: ctx => {
              const m = STAT_META[tlSelected[ctx.datasetIndex]];
              return " " + m.label + ": " + (ctx.raw != null ? m.fmt(ctx.raw) : "—");
            }
          }
        }
      },
      scales: {
        x:  { grid: { color: "rgba(0,141,185,0.08)" }, ticks: { color: "#7a9ab5", font: { size: 11 } } },
        yR: { display: hasR, position: "left",
              grid: { color: "rgba(0,141,185,0.08)" },
              ticks: { color: "#7a9ab5", font: { size: 11 }, callback: v => v.toFixed(3) } },
        yC: { display: hasC, position: hasR ? "right" : "left",
              grid: { drawOnChartArea: !hasR, color: "rgba(0,141,185,0.08)" },
              ticks: { color: "#7a9ab5", font: { size: 11 }, callback: v => Math.round(v) } },
      }
    }
  });

  document.getElementById("tl-legend").innerHTML = tlSelected.map((k, si) =>
    "<div class=\"legend-item\"><div class=\"legend-line\" style=\"background:" + COLORS[si] + "\"></div>" + STAT_META[k].label + "</div>"
  ).join("");

  document.getElementById("tl-cards").innerHTML = tlSelected.map((key, si) => {
    const meta = STAT_META[key];
    const vals = pRows.map(r => safeNum(r[key])).filter(v => v != null);
    if (!vals.length) return "<div class=\"tl-card c" + si + "\"><div class=\"m-label\">" + meta.label + "</div><div class=\"m-value\">&#8212;</div></div>";
    const avg        = vals.reduce((a, b) => a + b, 0) / vals.length;
    const best       = Math.max(...vals);
    const bestIdx    = pRows.map(r => safeNum(r[key])).indexOf(best);
    const bestSeason = SEASONS[bestIdx] || "—";
    return "<div class=\"tl-card c" + si + "\">" +
      "<div class=\"m-label\">" + meta.label + "</div>" +
      "<div class=\"m-value\" style=\"font-size:20px;color:" + COLORS[si] + "\">" + meta.fmt(avg) + "</div>" +
      "<div class=\"m-sub\">career avg &nbsp;&middot;&nbsp; best " + meta.fmt(best) + " (" + bestSeason + ")</div></div>";
  }).join("");
}

// ═══════════════════════════════════════════════════════════
// TAB 4 — STAT RANKER
// ═══════════════════════════════════════════════════════════
let rkChart = null;

function renderRanker() {
  const statKey = document.getElementById("rk-stat").value;
  const season  = document.getElementById("rk-season").value;
  const minPA   = parseInt(document.getElementById("rk-minpa").value) || 0;
  const meta    = STAT_META[statKey];

  const rows = bySeason(season)
    .filter(r => r.PA >= minPA && safeNum(r[statKey]) != null)
    .sort((a, b) => b[statKey] - a[statKey]);

  const labels = rows.map(r => playerName(r));
  const values = rows.map(r => meta.isRate ? parseFloat((+r[statKey]).toFixed(4)) : Math.round(+r[statKey]));
  const maxV   = Math.max(...values) || 1;
  const minV   = Math.min(...values) || 0;

  const bgColors = values.map((v, i) => {
    if (i === 0) return "#F26522";
    if (i === 1) return "#c45218";
    if (i === 2) return "#8f3c12";
    const t = (v - minV) / (maxV - minV || 1);
    return "rgba(0,141,185," + (0.25 + t * 0.45).toFixed(2) + ")";
  });

  const wrapH = Math.max(200, rows.length * 38 + 60);
  document.querySelector("#pane-ranker .chart-wrap").style.height = wrapH + "px";

  if (rkChart) rkChart.destroy();
  rkChart = new Chart(document.getElementById("rk-chart"), {
    type: "bar",
    data: { labels, datasets: [{ data: values, backgroundColor: bgColors, borderRadius: 4, borderSkipped: false }] },
    options: {
      indexAxis: "y", responsive: true, maintainAspectRatio: false,
      plugins: {
        legend: { display: false },
        tooltip: {
          backgroundColor: "#0a0e13", titleColor: "#7a9ab5", bodyColor: "#e8edf2",
          borderColor: "rgba(0,141,185,0.3)", borderWidth: 1, padding: 10,
          callbacks: {
            label: ctx => {
              const p = rows[ctx.dataIndex];
              return [" " + meta.label + ": " + meta.fmt(p[statKey]), " PA: " + p.PA];
            }
          }
        }
      },
      scales: {
        x: {
          grid: { color: "rgba(0,141,185,0.08)" },
          ticks: {
            color: "#7a9ab5", font: { size: 11 },
            callback: v => statKey === "QABpct" ? (v*100).toFixed(0)+"%" : meta.isRate ? v.toFixed(3) : Math.round(v)
          }
        },
        y: {
          grid: { display: false },
          ticks: { color: "#7a9ab5", font: { size: 12 }, callback: (v, i) => "#"+(i+1)+"  "+labels[i] }
        }
      }
    }
  });

  const avg    = values.length ? values.reduce((a, b) => a + b, 0) / values.length : null;
  const sorted = [...values].sort((a, b) => a - b);
  const median = sorted.length ? sorted[Math.floor(sorted.length / 2)] : null;
  const spread = values.length ? maxV - minV : null;
  const leader = rows[0];
  const fmtV   = v => v != null ? meta.fmt(v) : "—";

  document.getElementById("rk-metrics").innerHTML = [
    { l: meta.label + " leader", v: leader ? playerName(leader) : "—", s: leader ? meta.fmt(leader[statKey]) : "" },
    { l: "Team average",         v: fmtV(avg),    s: rows.length + " players" },
    { l: "Median",               v: fmtV(median), s: "middle of roster" },
    { l: "Spread",               v: fmtV(spread), s: "best minus worst" },
  ].map(m => "<div class=\"metric-card accent-top\">" +
    "<div class=\"m-label\">" + m.l + "</div>" +
    "<div class=\"m-value\">" + m.v + "</div>" +
    "<div class=\"m-sub\">"  + m.s + "</div></div>").join("");
}

// ── INIT ──────────────────────────────────────────────────
function populateSeasons(selId) {
  const el = document.getElementById(selId);
  SEASONS.forEach(s => {
    const o = document.createElement("option");
    o.value = s; o.textContent = s; el.appendChild(o);
  });
  el.selectedIndex = Math.max(0, SEASONS.length - 1);
}

populateSeasons("sum-season");
populateSeasons("rk-season");
initTimeline();
renderSummary();
</script>
</body>
</html>]"

# ------------------------------------------------------------
# 6. Assemble — four chunks with three JSON injections
# ------------------------------------------------------------

html <- paste0(chunk1, data_json, chunk2, seasons_json, chunk3, alltime_json, chunk4)

output_path <- "SWAT_Boys_Dashboard.html"
writeLines(html, output_path, useBytes = TRUE)
output_index <- "index.html"
writeLines(html, output_index, useBytes = TRUE)

cat("\n Dashboard written to:", normalizePath(output_path), "\n")
cat(" File size:", round(file.size(output_path) / 1024, 1), "KB\n")
cat(" Open in any browser — no internet required after first load.\n")
cat(" Safe to email as an attachment.\n\n")
