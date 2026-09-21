// Audit test: exercises the architecture-analysis functions that were the
// regression-fix targets (dependencyGraph, byTier, domainCoupling,
// impactOfChange) plus summary — on both the full and the slim build.

async function exercise(mod, label) {
  const {
    register, resetRegistry,
    dependencyGraph, byTier, domainCoupling, impactOfChange, summary,
  } = mod;

  resetRegistry();

  register(:fn/authenticate, :atlas/execution-function,
    [:domain/auth, :tier/service],
    { "execution-function/deps": [:component/database] });
  register(:endpoint/sign-in, :atlas/interface-endpoint,
    [:domain/auth, :tier/api],
    { "interface-endpoint/deps": [:fn/authenticate] });
  register(:component/database, :atlas/structure-component,
    [:domain/auth, :tier/foundation],
    { "structure-component/deps": [] });

  console.log(`\n########## ${label} ##########`);

  console.log('-- dependencyGraph --');
  console.log(dependencyGraph(:atlas/dev-id, :execution-function/deps));

  console.log('-- byTier --');
  console.log(byTier(:atlas/dev-id));

  console.log('-- domainCoupling --');
  console.log(domainCoupling(:atlas/dev-id, :execution-function/deps));

  console.log('-- impactOfChange(:component/database) --');
  console.log(impactOfChange(:component/database, :atlas/dev-id,
    :execution-function/deps, :execution-function/response));

  console.log('-- summary --');
  console.log(summary());
}

// Datalog probe (full build only) — isolates the JVM-vs-cljs discrepancy in
// queryEntitiesWithAspect. Registers with explicit :atlas/dev-id to match the
// JVM ground-truth test exactly.
async function datalogProbe(mod) {
  const { register, resetRegistry, getRegistry, rebuildDatalog,
          queryEntitiesWithAspect } = mod;

  resetRegistry();
  register(:fn/authenticate, :atlas/execution-function, [:domain/auth, :tier/service],
    { "atlas/dev-id": :fn/authenticate,
      "execution-function/deps": [:component/database] });
  register(:component/database, :atlas/structure-component, [:domain/auth, :tier/foundation],
    { "atlas/dev-id": :component/database,
      "structure-component/deps": [] });

  console.log('\n########## DATALOG PROBE (full build) ##########');
  console.log('registry entity count:', Object.keys(getRegistry()).length);
  rebuildDatalog();
  console.log('queryEntitiesWithAspect(:domain/auth):', queryEntitiesWithAspect(:domain/auth));
}

async function main() {
  const full = await import('@semantic-namespace/atlas');
  await exercise(full, 'FULL BUILD (dist/atlas.js)');

  const slim = await import('@semantic-namespace/atlas/slim');
  await exercise(slim, 'SLIM BUILD (dist-slim/atlas.js)');

  await datalogProbe(full);

  console.log('\n>>> AUDIT OK: both builds exercised without error');
}

main().catch((e) => { console.error('AUDIT FAILED:', e); process.exit(1); });
