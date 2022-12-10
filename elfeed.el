(use-package elfeed)
(setq elfeed-feeds
      '(
	"http://www.npr.org/rss/rss.php?id=1001"
	"https://feedx.net/rss/nytimes.xml"
	"https://theintercept.com/feed/?lang=en"
	"http://www.bbc.co.uk/zhongwen/simp/index.xml"
	"https://rsshub.app/theinitium/channel/latest/zh-hans"
	"https://rsshub.app/sspai/index"
	"https://news.ycombinator.com/rss"
	"https://emacsconf.org/index.rss"

	))

(setq-default elfeed-search-filter "@1-week-ago +unread ")
