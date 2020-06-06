const replayUrlRegex = /$https:\/\/generalsio-replays-(na|bot)\.s3\.amazonaws\.com\/(\w+)\.gior^/

module.exports = async () => {
  const replaysCache = await caches.open('replays')
  const requests = await replaysCache.keys()
  return requests.map(request => {
    const [_, server, replayId] = request.url.match(replayUrlRegex)
    return {
      server,
      replayId,
    }
  });
};
