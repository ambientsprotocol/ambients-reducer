type t('a) = {
  source: 'a, 
  target: 'a, 
  capability: Capability.capability,
  cocapability: Capability.capability
};

let create (source, target, capability, cocapability) = {
  {source, target, capability, cocapability};
};
