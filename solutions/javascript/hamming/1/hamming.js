export function compute(strand1, strand2) {
    const length = strand1.length;
    let differentPieces = 0;
    if (length !== strand2.length)
        throw (new Error('strands must be of equal length'));
    for (let index = 0; index < length; index++)
        strand1[index] !== strand2[index] ? differentPieces++ : null;
    return differentPieces;
};
