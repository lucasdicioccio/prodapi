"use strict";

exports.tabUrl = f => () =>
	function(){
		if (typeof(chrome) === "undefined")
			return;
		if (chrome && chrome.tabs) {
			chrome.tabs.query({active: true, currentWindow: true}, function(tabs) {
				var url = new URL(tabs[0].url);
				f(url.protocol + '//' + url.host)();
			});
		}}();
