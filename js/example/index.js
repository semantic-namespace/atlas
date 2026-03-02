// Example: @semantic-namespace/atlas with :ns/name qualified keyword syntax
//
// The Babel plugin (js-qualified-keywords/babel-plugin) transforms :ns/name
// literals into kw() calls at build time, so you write keywords as you would
// in Clojure without any wrapper function.
//
// Run: npm start  (babel-node index.js)

async function main() {
  const {
    register,
    resetRegistry,
    findByAspect,
    findByDevId,
    aspectFrequency,
    summary,
  } = await import('@semantic-namespace/atlas');

  resetRegistry();

  // Register entities using qualified keyword syntax
  register(
    :fn/validate-token,
    :atlas/execution-function,
    [:domain/auth, :operation/validate, :tier/service],
    {
      "execution-function/context":  ["auth/token"],
      "execution-function/response": ["auth/valid?"],
    },
  );

  register(
    :fn/refresh-token,
    :atlas/execution-function,
    [:domain/auth, :operation/refresh, :tier/service],
    {
      "execution-function/context":  ["auth/refresh-token"],
      "execution-function/response": ["auth/token"],
    },
  );

  register(
    :comp/oauth,
    :atlas/structure-component,
    [:domain/auth, :tier/foundation],
    { "structure-component/provides": ["auth/token"] },
  );

  // Query
  console.log('=== findByAspect(:domain/auth) ===');
  console.log(findByAspect(:domain/auth));

  console.log('\n=== findByDevId(:fn/validate-token) ===');
  console.log(findByDevId(:fn/validate-token));

  console.log('\n=== aspectFrequency() ===');
  console.log(aspectFrequency());

  console.log('\n=== summary() ===');
  console.log(summary());
}

main().catch(console.error);
