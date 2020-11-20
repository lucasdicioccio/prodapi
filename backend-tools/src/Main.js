"use strict";

exports.tabUrl = f => () =>
  chrome.tabs.query({active: true, currentWindow: true}, function(tabs) {
    var url = new URL(tabs[0].url);
    f(url.protocol + '//' + url.host)();
  });
