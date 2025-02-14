from dataclasses import dataclass
from typing import Optional

@dataclass
class SearchResponse:
    videoId:  Optional[str] = None
    browseId: Optional[str] = None
    videoTitle: Optional[str] = None
    channelTitle: Optional[str] = None
    videoImage: Optional[str] = None
    channelImage: Optional[str] = None
    videoLength: Optional[str] = None
    videoViews: Optional[str] = None
    videoPublishedTime: Optional[str] = None