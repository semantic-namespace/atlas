'use strict';
// EXPERIMENTAL. A meta-source JS pack: the calls a JavaScript/TypeScript
// service makes to another service's endpoints, checked against that
// service's atlas registry.
//
// An endpoint call is a registry entry naming the endpoint it reaches:
//
//   {:endpoint-call/order {:atlas/type :atlas/endpoint-call
//                          :endpoint-call/endpoint :endpoint/order}}
//
// and the property of an endpoints table that builds its path carries the
// entry's id (meta-source's meta key, here :atlas.source/id):
//
//   export const SHOP_ENDPOINTS = {
//     /** @atlas.source/id :endpoint-call/order */
//     order: (u: string, o: string) => `/resource/shop.users.${u}.orders/${o}`,
//   };
//
// The upstream's endpoints (a JSON export of its registry: method, path,
// input, output) become entities here, so `where`, `uses` and `deps` connect
// a call to its endpoint. With a checkout of the upstream (:upstream :root),
// each endpoint is also located in it: its registry entry, and the var
// carrying its id. Options, under the pack in meta-source.edn:
//
//   {:entry-type :atlas/endpoint-call        ; the calls' :atlas/type
//    :reference-key :endpoint-call/endpoint  ; the key naming the endpoint
//    :upstream {:name "shop"
//               :endpoints "atlas/upstream/shop.endpoints.json"
//               :root "../shop" :registry ["resources/registry"] :src ["src"]
//               :id-key :atlas.source/id}}
//
// What check reports:
//   path-drift        error    a tagged property builds a path other than its endpoint's
//   method-drift      error    a caller uses a client method other than its endpoint's
//   unknown-endpoint  error    an entry names an endpoint the upstream doesn't have
//   entry-without-tag warning  an entry of the type no code is tagged with
//   untagged-match    info     an untagged property of a tagged table builds exactly one endpoint's path
//   not-in-registry   info     an untagged property builds a path no upstream endpoint declares
const fs = require('fs');
const path = require('path');

const METHODS = new Set(['get', 'post', 'put', 'patch', 'delete']);
const PARAM = '\u0001';

// =============================================================================
// Options
// =============================================================================

function options(ctx) {
  const { kw, edn } = ctx;
  const o = ctx.opts instanceof Map ? ctx.opts : new Map();
  const up = o.get(kw(':upstream')) instanceof Map ? o.get(kw(':upstream')) : new Map();
  const str = (m, k, d) => (m.get(kw(k)) == null ? d : String(m.get(kw(k))));
  const strs = (m, k, d) => (m.get(kw(k)) == null ? d : edn.itemsOf(m.get(kw(k))).map(String));
  const kwOf = (m, k, d) => (m.get(kw(k)) == null ? kw(d) : kw(String(m.get(kw(k)))));
  return {
    entryType: kwOf(o, ':entry-type', ':atlas/endpoint-call'),
    referenceKey: kwOf(o, ':reference-key', ':endpoint-call/endpoint'),
    upstream: {
      name: str(up, ':name', 'upstream'),
      endpoints: up.get(kw(':endpoints')) == null ? null : path.resolve(ctx.root, String(up.get(kw(':endpoints')))),
      root: up.get(kw(':root')) == null ? null : path.resolve(ctx.root, String(up.get(kw(':root')))),
      registry: strs(up, ':registry', ['registry']),
      src: strs(up, ':src', ['src']),
      idKey: kwOf(up, ':id-key', ':atlas.source/id').text,
    },
  };
}

const cache = new Map(); // what reading the upstream gave, by its files' mtimes

/** {id-text -> endpoint} from the upstream's JSON export, with where each is written. */
function upstream(ctx, o) {
  const u = o.upstream;
  if (!u.endpoints || !fs.existsSync(u.endpoints)) return { endpoints: new Map(), problem: u.endpoints && `${ctx.relative(u.endpoints)} is not there` };
  const key = [u.endpoints, fs.statSync(u.endpoints).mtimeMs, u.root].join('|');
  if (cache.has(key)) return cache.get(key);
  const text = fs.readFileSync(u.endpoints, 'utf8');
  const json = JSON.parse(text);
  const lines = text.split('\n');
  const endpoints = new Map();
  for (const [id, e] of Object.entries(json.endpoints || {})) {
    const line = lines.findIndex((l) => l.includes(JSON.stringify(id) + ':') || l.includes(JSON.stringify(id) + ' :')) + 1;
    endpoints.set(':' + id, { ...e, sources: [{ file: u.endpoints, line: line || 1 }] });
  }
  // in a checkout of the upstream: its registry entry, and the var carrying its id
  if (u.root && fs.existsSync(u.root)) {
    for (const f of filesUnder(u.root, u.registry, '.edn')) {
      let rows;
      try {
        rows = ctx.edn.keyedLines(fs.readFileSync(f, 'utf8')) || [];
      } catch (_) {
        continue;
      }
      for (const r of rows) {
        const e = r.key && endpoints.get(r.key.text);
        if (e) e.sources = [{ file: f, line: r.line }];
      }
    }
    const tag = new RegExp(`${escape(u.idKey)}\\s+(:[\\w.\\-*+!?<>=]+\\/[\\w.\\-*+!?<>=]+)`, 'g');
    for (const f of filesUnder(u.root, u.src, ['.clj', '.cljc', '.cljs'])) {
      const fl = fs.readFileSync(f, 'utf8').split('\n');
      fl.forEach((l, i) => {
        for (const m of l.matchAll(tag)) {
          const e = endpoints.get(m[1]);
          if (e) e.impl = { file: f, line: i + 1 };
        }
      });
    }
  }
  const out = { endpoints };
  cache.clear();
  cache.set(key, out);
  return out;
}

const escape = (s) => s.replace(/[.*+?^${}()|[\]\\/]/g, '\\$&');

function filesUnder(root, dirs, exts) {
  const ends = [].concat(exts);
  const out = [];
  const visit = (f) => {
    let st;
    try {
      st = fs.statSync(f);
    } catch (_) {
      return;
    }
    if (st.isDirectory()) {
      if (!path.basename(f).startsWith('.') && path.basename(f) !== 'node_modules') for (const n of fs.readdirSync(f)) visit(path.join(f, n));
    } else if (ends.some((e) => f.endsWith(e))) out.push(f);
  };
  for (const d of dirs) visit(path.resolve(root, d));
  return out.sort();
}

// =============================================================================
// Paths
// =============================================================================

/** The text an endpoints-table value builds, each ${...} as PARAM. */
function templateText(ts, node) {
  while (node && (ts.isParenthesizedExpression(node) || ts.isAsExpression(node))) node = node.expression;
  if (!node) return null;
  if (ts.isArrowFunction(node) || ts.isFunctionExpression(node) || ts.isMethodDeclaration(node)) {
    let b = node.body;
    if (b && ts.isBlock(b)) {
      const ret = b.statements.find(ts.isReturnStatement);
      b = ret && ret.expression;
    }
    return templateText(ts, b);
  }
  if (ts.isStringLiteral(node) || ts.isNoSubstitutionTemplateLiteral(node)) return node.text;
  if (ts.isTemplateExpression(node)) return node.head.text + node.templateSpans.map((s) => PARAM + s.literal.text).join('');
  return null;
}

/** "/resource/shop.users.${u}/orders" -> ["shop", "users", PARAM, "orders"]: a resource's namespace splits on dots. */
function segments(text) {
  if (text == null) return null;
  const t = text.split('?')[0].replace(/^\^/, '').replace(/^\/+/, '');
  const parts = t.startsWith('resource/')
    ? (() => {
        const rest = t.slice('resource/'.length);
        const slash = rest.indexOf('/');
        return [...(slash < 0 ? rest : rest.slice(0, slash)).split('.'), ...(slash < 0 ? [] : rest.slice(slash + 1).split('/'))];
      })()
    : t.split('/');
  return parts.filter((s) => s !== '').map((s) => (s.includes(PARAM) ? PARAM : s));
}

const show = (segs) => (segs ? '/' + segs.map((s) => (s === PARAM ? '${…}' : s)).join('/') : '—');

/** strong: every segment agrees; weak: a ${…} stands where the path has a literal. */
function compare(segs, endpointPath) {
  if (!segs || !endpointPath) return { ok: false };
  const reg = endpointPath.split('/').filter((s) => s !== '');
  if (reg.length !== segs.length) return { ok: false };
  let weak = false;
  for (let k = 0; k < reg.length; k++) {
    if (reg[k].startsWith(':')) continue;
    if (segs[k] === PARAM) weak = true;
    else if (segs[k] !== reg[k]) return { ok: false };
  }
  return { ok: true, weak };
}

// =============================================================================
// The code: tables, their properties' paths, and the client method each caller uses
// =============================================================================

function nodeAt(ts, sf, line, col) {
  const pos = sf.getPositionOfLineAndCharacter(line - 1, col - 1);
  let found = null;
  (function visit(n) {
    if (pos < n.getStart(sf) || pos >= n.end) return;
    if (ts.isIdentifier(n) && n.getStart(sf) === pos) found = n;
    ts.forEachChild(n, visit);
  })(sf);
  return found;
}

const isClientCall = (ts, n, except) =>
  ts.isCallExpression(n) && ts.isPropertyAccessExpression(n.expression) && n.expression !== except && METHODS.has(n.expression.name.text);

/** The client method a table value is passed to, directly or through one variable. */
function clientMethod(ts, node) {
  let n = node;
  for (let up = 0; up < 8 && n.parent; up++) {
    n = n.parent;
    if (isClientCall(ts, n, node.parent && node.parent.parent)) return n.expression.name.text;
    if (ts.isVariableDeclaration(n) && ts.isIdentifier(n.name)) return methodUsing(ts, n);
    if (ts.isStatement(n)) return null;
  }
  return null;
}

function methodUsing(ts, decl) {
  const name = decl.name.text;
  let block = decl.parent;
  while (block && !ts.isBlock(block) && !ts.isSourceFile(block)) block = block.parent;
  const names = (n) => {
    let hit = false;
    (function v(m) {
      if (hit) return;
      if ((ts.isIdentifier(m) && m.text === name && m !== decl.name) || (ts.isShorthandPropertyAssignment(m) && m.name.text === name)) hit = true;
      else ts.forEachChild(m, v);
    })(n);
    return hit;
  };
  let found = null;
  (function visit(m) {
    if (found || !block) return;
    if (m.pos > decl.end && isClientCall(ts, m) && m.arguments.some(names)) found = m.expression.name.text;
    else ts.forEachChild(m, visit);
  })(block);
  return found;
}

/** Each property of a table with a tagged property: its path, and how it is called. */
function tables(ctx) {
  const { ts } = ctx;
  const vars = ctx.vars().filter((v) => v.table);
  const tagged = new Set(vars.filter((v) => v.devId).map((v) => `${v.file}#${v.table}`));
  return vars.filter((v) => tagged.has(`${v.file}#${v.table}`)).map((v) => {
    const sf = ctx.sourceFile(v.file);
    const id = nodeAt(ts, sf, v.nameAt.line, v.nameAt.col);
    const prop = id && id.parent;
    const value = prop && (ts.isPropertyAssignment(prop) ? prop.initializer : ts.isMethodDeclaration(prop) ? prop : null);
    const calls = ctx.refsTo(v).map((r) => {
      const rsf = ctx.sourceFile(r.file);
      const n = rsf && nodeAt(ts, rsf, r.line, r.col);
      // the property name in T.key: the reference points at T
      const access = n && ts.isPropertyAccessExpression(n.parent) ? n.parent : n;
      return { ...r, method: access ? clientMethod(ts, access) : null, keyCol: access && access !== n ? rsf.getLineAndCharacterOfPosition(access.name.getStart(rsf)).character + 1 : r.col };
    });
    return { v, segs: segments(value ? templateText(ts, value) : null), calls };
  });
}

// =============================================================================
// The pack
// =============================================================================

function referenceOf(ctx, o, entry) {
  const ref = entry instanceof Map ? entry.get(o.referenceKey) : null;
  return ref instanceof ctx.edn.Keyword ? ref : null;
}

function matches(endpoints, segs) {
  if (!segs) return [];
  const all = [...endpoints].map(([id, e]) => [id, e, compare(segs, e.path)]).filter(([, , c]) => c.ok);
  const strong = all.filter(([, , c]) => !c.weak);
  return strong.length ? strong : all;
}

const pack = {
  id: 'endpoint-call',

  /** The upstream's endpoints, as entities: located in its checkout when there is one. */
  entities(ctx) {
    const o = options(ctx);
    const { kw } = ctx;
    const out = [];
    for (const [id, e] of upstream(ctx, o).endpoints) {
      const entry = new Map([
        [kw(':endpoint/method'), kw(':' + e.method)],
        [kw(':endpoint/path'), e.path],
        [kw(':endpoint/input'), (e.input || []).map((k) => kw(':' + k))],
        [kw(':endpoint/output'), (e.output || []).map((k) => kw(':' + k))],
        [kw(':endpoint/upstream'), o.upstream.name],
      ]);
      out.push({ id, file: ctx.relative(e.sources[0].file), line: e.sources[0].line, entry });
      if (e.impl) out.push({ id, file: ctx.relative(e.impl.file), line: e.impl.line, entry: new Map() });
    }
    return out;
  },

  check(ctx) {
    const o = options(ctx);
    const { kw, edn } = ctx;
    const up = upstream(ctx, o);
    const pr = edn.prStr;
    const out = [];
    if (up.problem) out.push({ severity: 'error', problem: 'upstream-not-read', message: up.problem });
    const entities = ctx.entities();
    const ts = tables(ctx);
    const byId = new Map(ts.filter((t) => t.v.devId).map((t) => [t.v.devId, t]));

    for (const [id, e] of entities) {
      if (e.entry.get(kw(':atlas/type')) !== o.entryType) continue;
      const d = e.definitions[0];
      const ref = referenceOf(ctx, o, e.entry);
      const ep = ref && up.endpoints.get(ref.text);
      if (!ctx.implOf(id))
        out.push({ severity: 'warning', problem: 'entry-without-tag', 'dev-id': id, file: d.file, line: d.line,
                   message: `${pr(id)} has no code tagged with it` });
      if (ref && !ep)
        out.push({ severity: 'error', problem: 'unknown-endpoint', 'dev-id': id, file: d.file, line: d.line,
                   message: `${pr(ref)} is not an endpoint of ${o.upstream.name}` });
      const t = byId.get(id.text);
      if (!ep || !t) continue;
      if (t.segs && !compare(t.segs, ep.path).ok)
        out.push({ severity: 'error', problem: 'path-drift', 'dev-id': id, file: ctx.relative(t.v.file), line: t.v.nameAt.line,
                   col: t.v.nameAt.col, 'end-col': t.v.nameAt.col + t.v.key.length,
                   message: `${t.v.key} builds ${show(t.segs)}, but ${pr(ref)} is ${ep.method.toUpperCase()} ${ep.path} in ${o.upstream.name}` });
      for (const c of t.calls)
        if (c.method && c.method !== ep.method)
          out.push({ severity: 'error', problem: 'method-drift', 'dev-id': id, file: ctx.relative(c.file), line: c.line,
                     col: c.keyCol, 'end-col': c.keyCol + t.v.key.length,
                     message: `${c.method.toUpperCase()} here, but ${pr(ref)} is ${ep.method.toUpperCase()} ${ep.path}` });
    }

    for (const t of ts.filter((x) => !x.v.devId)) {
      let m = matches(up.endpoints, t.segs);
      // a path two endpoints share (a GET and a POST) is told apart by how it is called
      const used = new Set(t.calls.map((c) => c.method).filter(Boolean));
      if (m.length > 1 && used.size === 1) m = m.filter(([, e]) => used.has(e.method));
      const at = { file: ctx.relative(t.v.file), line: t.v.nameAt.line, col: t.v.nameAt.col, 'end-col': t.v.nameAt.col + t.v.key.length };
      if (m.length === 1)
        out.push({ severity: 'info', problem: 'untagged-match', ...at, endpoint: kw(m[0][0]),
                   message: `${t.v.key} builds the path of ${m[0][0]} (${m[0][1].method.toUpperCase()} ${m[0][1].path}): tag it to have it checked` });
      else if (t.segs && m.length === 0 && up.endpoints.size)
        out.push({ severity: 'info', problem: 'not-in-registry', ...at,
                   message: `${t.v.key} builds ${show(t.segs)}, which no ${o.upstream.name} endpoint declares` });
    }
    return out;
  },

  /** The line a hover shows under an id: what a call reaches, or what an endpoint is. */
  describe(ctx, id, entry) {
    const o = options(ctx);
    const { kw, edn } = ctx;
    if (entry.get(kw(':endpoint/upstream')))
      return `${entry.get(kw(':endpoint/upstream'))} endpoint  ·  ${entry.get(kw(':endpoint/method')).name.toUpperCase()} ${entry.get(kw(':endpoint/path'))}`;
    const ref = referenceOf(ctx, o, entry);
    if (!ref) return null;
    const ep = upstream(ctx, o).endpoints.get(ref.text);
    return ep ? `calls ${ep.method.toUpperCase()} ${ep.path}  →  ${edn.prStr(ref)}` : `calls ${edn.prStr(ref)}: not an endpoint of ${o.upstream.name}`;
  },
};

module.exports = pack;
module.exports.segments = segments;
module.exports.compare = compare;
module.exports.PARAM = PARAM;
