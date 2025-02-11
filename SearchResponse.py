from dataclasses import dataclass

@dataclass
class SearchResponse:
    videoId : str
    browseId : str
    videoTitle : str
    channelTitle : str
    videoImage : str
    channelImage : str
    videoLength : str
    videoViews : str
    videoPublishedTime : str