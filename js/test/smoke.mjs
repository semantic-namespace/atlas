import assert from 'node:assert/strict';
import { readFileSync } from 'node:fs';

const store = {
  'acme/shop/entities/atlas_execution-function.edn':
    '[#{:atlas/execution-function :domain/cart}\n {:atlas/dev-id :fn/add-item\n  :atlas/type :atlas/execution-function\n  :execution-function/context [:cart/id :cart/item]\n  :execution-function/response [:cart/total]}]\n' +
    '[#{:atlas/execution-function :domain/billing}\n {:atlas/dev-id :fn/charge\n  :atlas/type :atlas/execution-function\n  :execution-function/context [:cart/total]\n  :execution-function/response [:billing/receipt]}]\n',
  'acme/shop/entities/atlas_structure-component.edn':
    '[#{:atlas/structure-component :domain/billing}\n {:atlas/dev-id :component/gateway\n  :atlas/type :atlas/structure-component}]\n',
  'acme/shop/README.md': 'not an entity file',
};

for (const build of ['../dist-slim/atlas.js', '../dist/atlas.js']) {
  const atlas = await import(new URL(build, import.meta.url));

  assert.equal(atlas.loadStore(store), 3, `${build}: loadStore(object) skips non-entity paths`);
  assert.equal(atlas.loadStore(Object.values(store).slice(0, 2)), 3, `${build}: loadStore(array)`);

  const [, props] = atlas.findByDevId('fn/charge');
  assert.deepEqual(props['execution-function/context'], ['cart/total'], `${build}: findByDevId`);
  assert.equal(Object.keys(atlas.findByAspect('domain/billing')).length, 2, `${build}: findByAspect`);
  assert.equal(
    Object.keys(atlas.findConsumers('cart/total', 'execution-function/context')).length, 1,
    `${build}: findConsumers`,
  );

  if (atlas.queryConsumersOf) {
    assert.ok(atlas.queryConsumersOf('cart/total').length > 0, `${build}: datalog queryConsumersOf is not empty`);
  }
  console.log(`ok ${build}`);
}

const pkg = JSON.parse(readFileSync(new URL('../package.json', import.meta.url), 'utf8'));
assert.deepEqual(Object.keys(pkg.dependencies ?? {}), [], 'no runtime dependencies');
console.log('ok package has no runtime dependencies');
