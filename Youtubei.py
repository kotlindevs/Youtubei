import requests
from dataclasses import dataclass
from typing import Optional
import pandas as pd


@dataclass
class Video():
    videoId: str
    browseId: str
    title: str
    channelTitle: str
    views: Optional[str] = None
    length: Optional[str] = None


Videos = list()


def sectionListRenderer(searchResponse):
    if "contents" in searchResponse:
        contents = searchResponse['contents']
        twoColumnSearchResultsRenderer = contents['twoColumnSearchResultsRenderer']
        primaryContents = twoColumnSearchResultsRenderer['primaryContents']
        sectionListRenderer = primaryContents['sectionListRenderer']
        return sectionListRenderer
    elif "continuationContents" in searchResponse:
        sectionListContinuation = searchResponse['continuationContents']['sectionListContinuation']
        return sectionListContinuation


def continuationsToken(sectionListRenderer):
    if "continuations" in sectionListRenderer:
        continuations = sectionListRenderer['continuations'][0]
        nextContinuationData = continuations['nextContinuationData']
        continuation = nextContinuationData['continuation']
        return continuation


def contents(sectionListRenderer):
    if "contents" in sectionListRenderer:
        contents = sectionListRenderer['contents'][0]
        itemSectionRenderer = contents['itemSectionRenderer']
        contents = itemSectionRenderer['contents']
        for content in contents:
            if "videoRenderer" in content:
                videoRenderer = content['videoRenderer']
                videoId = videoRenderer['videoId']
                browseId = videoRenderer['longBylineText']['runs'][0]['navigationEndpoint']['browseEndpoint']['browseId']
                title = videoRenderer['title']['runs'][0]['text']
                channelTitle = videoRenderer['longBylineText']['runs'][0]['text']
                length = videoRenderer.get('lengthText', {}).get('simpleText')
                views = videoRenderer.get(
                    'viewCountText', {}).get('simpleText')
                video = Video(
                    videoId=videoId,
                    browseId=browseId,
                    title=title,
                    channelTitle=channelTitle,
                    length=length,
                    views=views
                )
                if video not in Videos:
                    Videos.append(video)

            if "shelfRenderer" in content:
                shelfRenderer = content['shelfRenderer']
                content = shelfRenderer['content']
                verticalListRenderer = content.get('verticalListRenderer', {})
                items = verticalListRenderer.get('items', {})
                for item in items:
                    if "videoRenderer" in item:
                        videoRenderer = item['videoRenderer']
                        videoId = videoRenderer['videoId']
                        browseId = videoRenderer['longBylineText']['runs'][0]['navigationEndpoint']['browseEndpoint']['browseId']
                        title = videoRenderer['title']['runs'][0]['text']
                        channelTitle = videoRenderer['longBylineText']['runs'][0]['text']
                        length = videoRenderer.get(
                            'lengthText', {}).get('simpleText')
                        views = videoRenderer.get(
                            'viewCountText', {}).get('simpleText')
                        video = Video(
                            videoId=videoId,
                            browseId=browseId,
                            title=title,
                            channelTitle=channelTitle,
                            length=length,
                            views=views
                        )
                        if video not in Videos:
                            Videos.append(video)


url = "https://www.youtube.com"
ytSearch = url + "/youtubei/v1/search"

header = {
    "Content-Type": "application/json"
}

userInput = input("Search something : ")
query = userInput
params = "EgWKAQIQAWoKEAkQChAFEAMQBA%3D%3D"


body = {
    "context": {
        "client": {
            "clientName": "WEB",
            "clientVersion": "1.20241111.01.00"
        }
    }, "query": query, "params": params
}

youtube = requests.post(
    url=ytSearch, headers=header, json=body
)

if (youtube.status_code == 200):
    response = youtube.json()
    sectionList = sectionListRenderer(response)
    token = continuationsToken(sectionList)
    content = contents(sectionList)

    if sectionList is not None:
        while True:
            body['continuation'] = token
            youtube = requests.post(
                url=ytSearch, headers=header, json=body
            )
            if (youtube.status_code == 200):
                response = youtube.json()
                sectionList = sectionListRenderer(response)
                token = continuationsToken(sectionList)
                content = contents(sectionList)
                print(f"Results found : {len(Videos)}")
                df = pd.DataFrame(Videos)
                print(df)
                if token is None:
                    break
