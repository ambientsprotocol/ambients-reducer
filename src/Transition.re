type capability = Capability.t;

type t('a) = {
  source: 'a, 
  target: 'a, 
  capability: capability
};
