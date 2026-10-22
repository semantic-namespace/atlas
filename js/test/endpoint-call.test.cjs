'use strict';
// The endpoint-call pack, run by meta-source's JavaScript reader on
// fixtures/shop-node: a node service calling fixtures/shop-clj's endpoints.
const test = require('node:test');
const assert = require('node:assert');
const path = require('path');
const index = require('meta-source/js/src/index');
const hover = require('meta-source/js/src/hover');
const { kw, toJson } = require('meta-source/js/src/edn');
const { segments, compare, PARAM } = require('../meta-source/endpoint-call.cjs');

const build = (dir) => index.build(index.config(path.join(__dirname, 'fixtures', dir)));
const idx = build('shop-node');
const problems = toJson(index.check(idx));
const of = (p) => problems.filter((x) => x.problem === p);

test('a tagged property whose path and callers agree with its endpoint has no problem', () => {
  assert.deepStrictEqual(problems.filter((p) => p['dev-id'] === 'endpoint-call/order'), []);
});

test('path-drift: the property builds another path than its endpoint\'s', () => {
  const [p] = of('path-drift');
  assert.strictEqual(p['dev-id'], 'endpoint-call/refund');
  assert.match(p.message, /refund builds \/shop\/orders\/\$\{…\}\/refunds, but :endpoint\.post\/refund is POST \/shop\/orders\/:order-id\/refund/);
  assert.deepStrictEqual([p.file, p.line, p.col, p['end-col']], ['src/api/endpoints.ts', 7, 3, 9]);
});

test('method-drift: a caller uses another method, through a variable too', () => {
  const [p] = of('method-drift');
  assert.strictEqual(p['dev-id'], 'endpoint-call/place-order');
  assert.match(p.message, /^GET here, but :endpoint\.post\/orders is POST/);
  assert.deepStrictEqual([p.file, p.line], ['src/orders/service.ts', 11]);
});

test('an entry naming no upstream endpoint, and one no code is tagged with', () => {
  assert.deepStrictEqual(of('unknown-endpoint').map((p) => p['dev-id']), ['endpoint-call/gone']);
  assert.deepStrictEqual(of('entry-without-tag').map((p) => p['dev-id']), ['endpoint-call/gone']);
});

test('untagged properties: one endpoint\'s path (told apart by the method used), or none\'s', () => {
  assert.deepStrictEqual(of('untagged-match').map((p) => [p.message.split(' ')[0], p.endpoint]),
                         [['cart', 'endpoint/cart'], ['orders', 'endpoint.post/orders']]);
  assert.deepStrictEqual(of('not-in-registry').map((p) => p.message.split(' ')[0]), ['legacy']);
});

test('an upstream endpoint is an entity: its registry entry and its var in the checkout', () => {
  const w = toJson(index.where(idx, kw(':endpoint/order')));
  assert.deepStrictEqual(w.sources.map((s) => [s.file, s.line]), [['../shop-clj/registry/endpoints.edn', 1], ['../shop-clj/src/shop/api.clj', 3]]);
  assert.strictEqual(w.entry['endpoint/path'], '/shop/users/:user-id/orders/:order-id');
  assert.deepStrictEqual(toJson(index.uses(idx, kw(':endpoint/order')))['referenced-by'].map((r) => [r['dev-id'], r.via]),
                         [['endpoint-call/order', 'endpoint-call/endpoint']]);
});

test('with no checkout, an endpoint is located in the JSON export', () => {
  const alone = build('shop-node-alone');
  const w = toJson(index.where(alone, kw(':endpoint/order')));
  assert.deepStrictEqual(w.sources.map((s) => [s.file, s.line]), [['../shop-node/atlas/upstream/shop.endpoints.json', 3]]);
  assert.deepStrictEqual(toJson(index.check(alone)).map((p) => p.problem).filter((p) => p.endsWith('drift')), ['method-drift', 'path-drift']);
});

test('hovers say what a call reaches, and what an endpoint is', () => {
  assert.match(hover.lookup(idx, kw(':endpoint-call/order')).eldoc,
               /^:endpoint-call\/order {2}entry\ncalls GET \/shop\/users\/:user-id\/orders\/:order-id {2}→ {2}:endpoint\/order/);
  assert.match(hover.lookup(idx, kw(':endpoint/order')).eldoc, /^:endpoint\/order {2}endpoint-call\nshop endpoint {2}· {2}GET \/shop\/users/);
});

test('paths: a resource namespace splits on dots; a :param takes anything, a ${…} a literal only weakly', () => {
  assert.deepStrictEqual(segments(`/resource/shop.users.${PARAM}/orders?x=1`), ['shop', 'users', PARAM, 'orders']);
  assert.deepStrictEqual(compare(['shop', 'users', PARAM, 'orders'], '/shop/users/:user-id/orders'), { ok: true, weak: false });
  assert.deepStrictEqual(compare(['shop', PARAM, 'orders'], '/shop/users/orders'), { ok: true, weak: true });
  assert.strictEqual(compare(['shop', 'carts'], '/shop/orders').ok, false);
});
