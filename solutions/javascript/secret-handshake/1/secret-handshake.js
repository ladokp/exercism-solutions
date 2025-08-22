const handshakeCommands = Object.freeze([
  'wink',
  'double blink',
  'close your eyes',
  'jump'
]);

export const commands = (handshakeCode) => {
  if (typeof handshakeCode !== 'number')
    throw new Error('Handshake must be a number');

  const handshakes = handshakeCommands.filter(
    (_, index) => handshakeCode & Math.pow(2, index),
  );

  if (handshakeCode & Math.pow(2, 4)) handshakes.reverse();

  return handshakes;
};
