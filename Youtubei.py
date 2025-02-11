import requests
from SearchResponse import SearchResponse
from flask import Flask,request,jsonify

BASE_URL = "https://www.youtube.com"
SEARCH = "/youtubei/v1/search"
RESPONSE_OKAY = 200

headers = {
    "content-type"   : "application/json",
    "X-Goog-Api-Key" : "AIzaSyAO_FJ2SlqU8Q4STEHLGCilw_Y9_11qcW8",
    "prettyPrint"    : "false"
}

app = Flask(__name__)

def searchData(query):
    videoList = []
    body = {
        "context":{
            "client":{
                "clientName":"WEB",
                "clientVersion":"1.20241111.01.00"
            }
        },"query": query
    }
    
    try:
        client = requests.post(
            url=BASE_URL+SEARCH,headers=headers,json=body
        )
        client.raise_for_status
        
        if client.status_code==RESPONSE_OKAY:
            data = client.json()
            contents = data['contents']
            twoColumnSearchResultsRenderer = contents['twoColumnSearchResultsRenderer']
            primaryContents = twoColumnSearchResultsRenderer['primaryContents']
            sectionListRenderer = primaryContents['sectionListRenderer']
            contents = sectionListRenderer['contents']
            itemSectionRenderer = contents[0]['itemSectionRenderer']
            contents = itemSectionRenderer['contents']
            
            for video in contents:
                if "videoRenderer" in video:
                    videoId = video['videoRenderer']['videoId']
                    videoImage = video['videoRenderer']['thumbnail']['thumbnails'][0]['url']
                    videoTitle = video['videoRenderer']['title']['runs'][0]['text']
                    channelTitle = video['videoRenderer']['longBylineText']['runs'][0]['text']
                    browseId = video['videoRenderer']['longBylineText']['runs'][0]['navigationEndpoint']['browseEndpoint']['browseId']
                    videoPublishedTime = video['videoRenderer']['publishedTimeText']['simpleText']
                    videoLength =  video['videoRenderer']['lengthText']['simpleText']
                    videoViews = video['videoRenderer']['shortViewCountText']['simpleText']
                    channelImage = video['videoRenderer']['channelThumbnail']['thumbnails'][0]['url']
                    
                    myVideo = SearchResponse(
                        videoId= videoId,
                        browseId= browseId,
                        videoTitle= videoTitle,
                        channelTitle= channelTitle,
                        videoImage= videoImage,
                        channelImage= channelImage,
                        videoLength= videoLength,
                        videoPublishedTime= videoPublishedTime,
                        videoViews= videoViews
                    )
                    
                    videoList.append(myVideo)
            return videoList
    except requests.exceptions.RequestException as exp:
        print(f"Error : {exp}")
    except ValueError as ve:
        print(f"Error decoding json : {ve}")

@app.route('/',methods=['GET'])
def index():
    return open("./index.html").read()

@app.route('/sendData',methods=['POST'])
def sendData():
    json    = request.get_json()
    search  = json.get('search')
    result  = searchData(search)
    return jsonify(result)

if __name__=='__main__':
    app.run(debug=True)