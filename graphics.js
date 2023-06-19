let renderer = new Renderer(

//de
`
float ball(vec3 p){
    if(length(p-ballPos) > 3.5){
        return sdSphere(p-ballPos, 3.);
    }
    p -= ballPos;
    p = ballRotMat*p;
    float rad = 3.;
    float inRad = 3.;
    
    float innerSphere = sdSphere(p, inRad);

    vec3 r1P = p;
    r1P = twist(r1P, 0.3);

    float ring1 = sdTorus(r1P, vec2(rad, 0.5));

    vec3 r2P = p;
    r2P = rotY(r2P, 3.141/3.);
    r2P = twist(r2P, 0.3);

    float ring2 = sdTorus(r2P, vec2(rad, 0.5));

    vec3 r3P = p;
    r3P = rotY(r3P, 3.141*2./3.);
    r3P = twist(r3P, 0.3);

    float ring3 = sdTorus(r3P, vec2(rad, 0.5));

    ring1 = min(ring1, ring2);

    ring1 = min(ring1, ring3);

    return max(innerSphere, -ring1);
}

float playerFace(vec3 p2){
    float head = sdSphere(p2 - vec3(0., 0.4, 0.), 0.2);
    float nose = sdSphere(p2 - vec3(0., 0.37, 0.22), 0.1);
    float ear1 = min(
        sdCapsule(p2, vec3(0.17, 0.45, -0.1), 1.1*vec3(0.15, 0.56, -0.4), 0.03),
        sdCapsule(p2, vec3(0.12, 0.5, -0.1), 1.1*vec3(0.15, 0.56, -0.4), 0.03)
    );
    float ear2 = min(
        sdCapsule(p2, vec3(-0.17, 0.45, -0.1), 1.1*vec3(-0.15, 0.56, -0.4), 0.03),
        sdCapsule(p2, vec3(-0.12, 0.5, -0.1), 1.1*vec3(-0.15, 0.56, -0.4), 0.03)
    );
    ear1 = min(ear1, ear2);

    float cheek1 = sdCapsule(p2, vec3(0.17, 0.36, -0.), vec3(0.07, 0.32, 0.17), 0.02);
    float cheek2 = sdCapsule(p2, vec3(-0.17, 0.36, -0.), vec3(-0.07, 0.32, 0.17), 0.02);
    cheek1 = min(cheek1, cheek2);

    float socket1 = sdSphere(p2 - vec3(0.15, 0.45, 0.08), 0.07);
    float socket2 = sdSphere(p2 - vec3(-0.15, 0.45, 0.08), 0.07);
    socket1 = min(socket1, socket2);

    float noseBone = sdCapsule(p2, vec3(0.035, 0.43, 0.27), vec3(-0.035, 0.43, 0.27), 0.03);

    float d = smin(head, nose, 0.1);
    d = smin(d, ear1, 0.1);


    d = smin(d, cheek1, 0.05);

    d = smin(d, noseBone, 0.05);

    d = max(d, -socket1);
    return d;
}

float playerLegs(vec3 p){
    vec3 hip1 = vec3(0.1, -0.1, -0.12); //hip right
    vec3 jr1 = vec3(0.15, -0.26, 0.05); //joint right 1, top knee
    vec3 jr2 = vec3(0.13, -0.35, -0.15); //joint right 2, ankle joint
    vec3 jr3 = vec3(0.13, -0.46, 0.); //joint right 3, foot point

    vec3 hip2 = vec3(-0.1, -0.1, -0.12); //hip right
    vec3 jl1 = vec3(-0.15, -0.26, 0.05); //joint right 1, top knee
    vec3 jl2 = vec3(-0.13, -0.35, -0.15); //joint right 2, ankle joint
    vec3 jl3 = vec3(-0.13, -0.46, 0.); //joint right 3, foot point

    float thighGap = 0.05 / 2.;

    float thigh1 = min(
        sdCapsule(p-vec3(0., thighGap, 0.), hip1, jr1, 0.015),
        sdCapsule(p-vec3(0., -thighGap, 0.), hip1, jr1, 0.015)
    );
    float shin1 = sdCapsule(p, jr1, jr2, 0.025);
    float ankle1 = sdCapsule(p, jr2, jr3, 0.025);

    float thigh2 = min(
        sdCapsule(p-vec3(0., thighGap, 0.), hip2, jl1, 0.015),
        sdCapsule(p-vec3(0., -thighGap, 0.), hip2, jl1, 0.015)
    );
    float shin2 = sdCapsule(p, jl1, jl2, 0.025);
    float ankle2 = sdCapsule(p, jl2, jl3, 0.025);

    thigh1 = min(thigh1, shin1);
    thigh1 = min(thigh1, ankle1);
    thigh1 = min(thigh1, thigh2);
    thigh1 = min(thigh1, shin2);
    thigh1 = min(thigh1, ankle2);

    return thigh1;
}

float playerBody(vec3 p){

    float rCone = sdRoundCone(p, vec3(0., 0.1, 0.), vec3(0., -0.1, -0.12), 0.18, 0.1);
    float bottom = p.y - (-0.1);
    float d = max(rCone, -bottom);
    d = rCone;
    return d;
}

float playerEyes(vec3 p){
    float eye1 = sdSphere(p - vec3(0.15, 0.45, 0.08), 0.05);
    float eye2 = sdSphere(p - vec3(-0.15, 0.45, 0.08), 0.05);
    eye1 = min(eye1, eye2);

    return eye1;
}
float playerJacket(vec3 p, float face){
    vec3 jacketCenter = vec3(0., -0.1, 0.);
    p -= jacketCenter;
    float lift = 1.2;
    vec3 p2 = bend(p.zyx, lift);
    p2 = p2.zyx;
    p2 = bend(p2, lift);
    vec3 p1 = p2/vec3(1.,12.,1.);
    float jacket = sdTorus(p1, vec2(.24, 0.04));
    float cutRad = 1.;
    float jacketIntersection = sdSphere(p - cutRad*normalize(vec3(0., -1.,1.)), cutRad);

    jacketIntersection = min(
        jacketIntersection, 
        sdSphere(p-vec3(0., 0.15, 0.2), 0.2)
    );

    jacketIntersection = min(
        jacketIntersection,
        sdSphere(abs(p)-vec3(2., -0.2, 0.), 1.75)
    );

    jacket = max(jacket, -jacketIntersection);

    jacket = max(jacket, -sdSphere(p - vec3(0., 0.4, 0.) + jacketCenter, 0.2));



    float d = max(jacket, -face);

    return d;
}

float player(vec3 p){
    vec3 p2 = p-playerPos;

    if(length(p-playerPos) > 1.2){
        return sdSphere(p2, 1.);
    }
    p2 = rotY(p2, -playerAngle.x);

    float face = playerFace(p2);
    float body = playerBody(p2);
    float eyes = playerEyes(p2);
    float legs = playerLegs(p2);
    float jacket = playerJacket(p2, face);

    float d = min(face, body);
    d = min(d, eyes);
    d = min(d, legs);
    d = min(d, jacket);

    return d;
}

float groundBone(vec3 p){
    float d = p.y;
    d = max(d, -sdSphere(p-vec3(0.,10.,0.), 12.5));
    return d;
}

float ground(vec3 p){
    float base = p.y;
    float tiles = groundBone(p);
    float d = min(base, tiles);
    return base;
}

float walls(vec3 p){
    float walls = -sdBox(p, vec3(30., 100., 50.));
    vec3 goalSize = vec3(17.5, 20., 10.);
    float goals = min(sdBox(p - vec3(0.,0.,50.), goalSize), sdBox(p - vec3(0.,0.,-50.), goalSize));
    walls = max(walls, -goals);
    return walls;
}

float de(vec3 p){
    float d = 100000.;
    float floor = ground(p);
    float walls = walls(p);
    float ball = ball(p);
    float player = player(p);

    d = min(floor, walls);
    d = min(d, ball);
    d = min(d, player);
    return d;
}
`, 



//color
`

vec3 col = vec3(0.,0.,0.);
if(dist <= MIN_DIST){
    vec3 norm = grad(p);

    col = norm;
    if(length(p - ballPos) <= 3.005){
        vec3 p2 = p-ballPos;


        col = vec3(0.,0.2,.2) +vec3(vec3(clamp(dot(normalize(vec3(-0.2, .4, -0.5)), norm), 0., 1.)));

        if(length(p2) > 2.99){
            col = vec3(0.1);
        }
    }

    if(player(p) < 0.005){
        vec3 p2 = p-playerPos;
        p2 = rotY(p2, -playerAngle.x);

        col = vec3(abs(norm));

        if(playerFace(p2) <= 0.005){
            col = 2.*vec3(0.1,0.,0.2);

            vec3 faceNorm = norm + 0.1*noise2Norm(3.*p2);

            col *= clamp(dot(norm, normalize(vec3(1., 1., 1.))), 0., 1.);
            if(p2.x > 0.){
                col = mix(col, stars(dir)+vec3(0.,0.3,0.3), abs(noise(10.*p2)));
            }

            float eyeDist = playerEyes(p2);

            col += (1./(1.+40.*eyeDist));
        }

        if(playerEyes(p2) <= 0.005){
            col = vec3(0.8);
        }

        else if(playerBody(p2) <= 0.005){
            float sternumWidth = 0.02;
            float ribGap = 0.04;
            float ribSize = 0.02;
            vec3 bodyNorm = norm;

            col = stars(dir) + noise(2.*dir)*vec3(0.1,0.,0.2);
            if(abs(p2.x) < sternumWidth){
                col = vec3(.8);
                col = 4.*vec3(0.1,0.,0.2);
                col -= (length(p2.x)/(3.*sternumWidth));
            }
            float ribNum = mod(p2.y+abs(p2.x)/5., ribGap+ribSize);
            if(ribNum < ribSize){
                col = vec3(1.);
                col = 4.*vec3(0.1,0.,0.2);
                col -= (1.-(ribNum/ribSize));
            }


            //col *= clamp(dot(norm, normalize(vec3(1.))), 0., 1.);
        }

        else if(playerLegs(p2) <= 0.005){
            col = 4. * vec3(0.1, 0., 0.2);
            col *= clamp(dot(norm, normalize(vec3(1.))), 0., 1.);
        }

        else if(playerJacket(p2, playerFace(p2)) <= 0.005){
            col = vec3(0.1, 0.1, 0.2);
            vec3 p3 = p2 / vec3(1., 12., 1.);
            vec3 jacketNorm = norm + 0.2*noise2Norm(3.*p3);
            col *= clamp(dot(norm, normalize(vec3(1.))), 0., 1.);
            if(p2.x > 0. && false){
                col = mix(col, stars(dir)+vec3(0.,0.3,0.3), 1.-abs(noise(4.*p2)));
            }
        }

        //col += .5*dot(norm, vec3(1., 1., 1.));
    }


    else if(ground(p) <= 0.002){
        col = vec3(0.1,0.1,0.1);
        float noiseGood = noise(p*50.)/8. + noise(p*25.)/4. + noise(p*12.5)/2.;
        float pat = pat2(p);
        float eps = 0.001/2.;
        col += pat*vec3(0.9,1.,1.);
        vec3 patNorm = normalize(vec3(
            (pat2(p - vec3(eps, 0., 0.)) - pat2(p + vec3(eps, 0., 0.)))/(2.*eps),
            1.,
            (pat2(p - vec3(0., 0., eps)) - pat2(p + vec3(0., 0., eps)))/(2.*eps)
        ));
        vec3 patLightDir = normalize(vec3(1.,1.,1.));
        vec3 patLight2Dir = normalize(vec3(-0.7, .2, -0.3));

        float distNum = (5./(length(p-playerPos + vec3(0., 2., 0.))*length(p-playerPos + vec3(0., 2., 0.))));
        distNum = pow(1.1, -length(p-playerPos + vec3(0., 2., 0.)))/1.1;

        float patLight = clamp(dot(patNorm, patLightDir)*distNum + 0.5*(1.-distNum), 0., 1.);
        patLight = clamp(patLight, 0., 1.);

        patLight = clamp(patLight, 0., 1.);
    
        col = vec3(.1)*(0.6+patLight/2.);
        col += pat2(p)/7.;
        //col += vec3(0.8,1.,1.) * pat2(p)*abs(sin(t));

        col += pow(clamp(dot(normalize(p-camPos), reflect(normalize(vec3(0.,15.,0.)-p), patNorm)), 0., 1.), 1.)/4.;


        if(pat3(p) == 1. && false){
            col = vec3(0.);
            col += pow(clamp(dot(normalize(p-camPos), reflect(patLightDir, patNorm)), 0., 1.), 32.);
        }

        col += (1./(1.+4.*walls(p)));
    }

    else if(walls(p) <= 0.005){
        col = vec3(1.,0.,1.) + dot(norm, normalize(vec3(1.)));
        col = stars(dir) + vec3(0.1,0.,0.2)*noise(dir*1.5);
        if(abs(norm.x) > 0.7){
            col += pat2(p.yxz)*(1./(p.y+1.));
        }
        if(abs(norm.z) > 0.7){
            col += pat2(p.xzy)*(1./(p.y+1.));
        }
    }

} else {
    color = vec4(0.,0.,0.,1.);
}

color = vec4(col, 1.);

color = pow(color, vec4(1.));
`, 




//functions for color
`
float pattern(vec2 p, float t, float o){
    p /= 1.5;
    
    float sum = 0.;

    for(float i = 0.; i < o; i++){
        p = fract(1.5*p)-.5;
        float d = sin(10.*length(p) + t);
        d = .1/d;
        sum += d;
    }

    return clamp(sum, 0., 1.);
}

float pat2(vec3 p){
    float noise1 = noise(p/10.)/2. + noise(p)/2. + noise(p*2.)/4. + noise(p*4.)/4. + noise(p*8.)/4. + noise(p*16.)/4.+noise(p*32.)/8.+noise(p*64.)/16.+noise(p*128.)/32.;

    //return 1.-pattern(p.xz/200., 72./20., 12.-length((playerPos.xz-p.xz))/(140./15.)) + noise(p)/1000.;
    //return 1.-pattern(p.xz/200., 72./20., 12.) + noise(p)/1000.;
    return pattern(p.xz/200., 45./20., 12.) + noise(2.*p)/200. + noise1/10.;
    //return pattern(p.xz/200., t/20., 12.) + noise(p)/1000.;

}

float pat3(vec3 p){
    return pattern(p.xz/200., 72./20., 12.);
}

float noise2(vec3 p){
    return noise(p/10.)/2. + noise(p)/2. + noise(p*2.)/4. + noise(p*4.)/4. + noise(p*8.)/4. + noise(p*16.)/4.+noise(p*32.)/8.+noise(p*64.)/16.+noise(p*128.)/32.;
}

vec3 noise2Norm(vec3 p){
    float eps = 0.01;
    return normalize(vec3(
        noise2(p + vec3(eps, 0., 0.)) - noise2(p + vec3(-eps, 0., 0.)),
        noise2(p + vec3(0., eps, 0.)) - noise2(p + vec3(0., -eps, 0.)),
        noise2(p + vec3(0., 0., eps)) - noise2(p + vec3(0., 0., -eps))
    ));
}

`
);

//vec3 goalSize = vec3(17.5, 20., 10.);
function de(p){
    let d = Infinity;
    let floor = p[1];
    let walls = -sdBox3(p, [30, 100, 50]);
    let goalSize = [17.5, 20, 10];
    let goals = min(
        sdBox3(plus(p, [0, 0, -50]), goalSize),
        sdBox3(plus(p, [0, 0, 50]), goalSize)
    );
    walls = max(walls, -goals);

    d = min(floor, walls);


    return d;
}